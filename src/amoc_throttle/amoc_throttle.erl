%%==============================================================================
%% @doc This module allows to synchronize the users and act on groups of them.
%%
%% Amoc throttle is a module that allows limiting the number of users' actions per given interval,
%% no matter how many users there are in a test.
%% It works in both local and distributed environments, allows for dynamic rate changes
%% during a test and exposes metrics which show the number of requests and executions.
%%
%% Amoc throttle allows setting the execution `Rate' per `Interval' or limiting the number
%% of parallel executions when `Interval' is set to `0'.
%% Each `Rate' is identified with a `Name'.
%% The rate limiting mechanism allows responding to a request only when it does not exceed the given `Rate'.
%% Amoc throttle makes sure that the given `Rate' per `Interval' is maintained on a constant level.
%% It prevents bursts of executions which could blurry the results,
%% as they technically produce a desired rate in a given interval.
%% Because of that, it may happen that the actual `Rate' would be slightly below the demanded rate.
%% However, it will never be exceeded.
%%
%% <h2>Examples</h2>
%%
%% A typical use of Amoc throttle will look something like this:
%%
%% ```
%% -module(scenario_with_throttle).
%%
%% -export([init/0]).
%% -export([start/1]).
%%
%% init() ->
%%     amoc_throttle:start(messages_rate, 100),        %% 100 messages per minute
%%     %% continue initialization
%%     ok.
%%
%% start(Id) ->
%%     %% initialize user
%%     user_loop(Id),
%%     ok.
%%
%% user_loop(Id) ->
%%     amoc_throttle:send_and_wait(messages_rate, some_message),
%%     send_message(Id),
%%     user_loop(Id).
%% '''
%%
%% Here a system should be under a continuous load of 100 messages per minute.
%% Note that if we used something like `amoc_throttle:run(messages_rate, fun() -> send_message(Id) end)'
%% instead of `send_and_wait/2' the system would be flooded with requests.
%%
%% A test may of course be much more complicated.
%% For example it can have the load changing in time.
%% A plan for that can be set for the whole test in `init()':
%%
%% ```
%% init() ->
%%     %% init metrics
%%     amoc_throttle:start(messages_rate, 100),
%%     %% 9 steps of 100 increases in Rate, each lasting one minute
%%     amoc_throttle:change_rate_gradually(messages_rate, 100, 1000, 60000, 60000, 9),
%%     ok.
%% '''
%%
%% Normal Erlang messages can be used to schedule tasks for users by themselves or by some controller process.
%% Below there's a sketch of a user's behaviour during a test.
%% It waits for messages in a loop, and sends one after receiving the message `{send_message, To}'.
%% The rate of messages sent during a test will not exceed the one set in the `message_rate'.
%% Sending messages is scheduled in the `set_up/1' function and in the user loop if some arbitrary condition is met.
%% This models the behaviour common across load tests, when users respond only to some messages.
%%
%% ```
%% set_up(Users) ->
%%     [User ! {send_message, To} || User <- Users, To <- Users].
%%
%% user_loop() ->
%%     receive
%%         {send_message, To} ->
%%             send_xmpp_message(To),
%%             user_loop();
%%         Message ->
%%             process_message(Message),
%%             user_loop()
%%     end.
%%
%% process_message(Message) ->
%%     case some_condition(Message) of
%%         true ->
%%             To = get_sender(Message),
%%             amoc_throttle:send(message_rate, {send_message, To});
%%         false ->
%%             ok
%%     end.
%% '''
%%
%% For a more comprehensive example please refer to the `throttle_test' scenario,
%% which shows possible usages of the Amoc throttle.
%%
%% <h2>How it works</h2>
%%
%% <h3>Module overview</h3>
%%
%% <ul>
%% <li> `amoc_throttle' - provides an API for `amoc_throttle_controller'.</li>
%% <li> `amoc_throttle_controller' - a `gen_server' which is responsible for reacting to requests,
%%      and managing `throttle_processes'. In a distributed environment an instance of `amoc_throttle_controller'
%%      runs on every node, and the one running on the master Amoc node stores the state for all nodes.</li>
%% <li> `amoc_throttle_process' - `gen_server' module, implements the logic responsible for
%%      limiting the rate. For every `Name', a `NoOfProcesses' are created, each responsible for
%%      keeping executions at a level proportional to their part of `Rate'.</li>
%% </ul>
%%
%% <h3>Distributed environment</h3>
%%
%% <h4>Metrics</h4>
%%
%% In a distributed environment every Amoc node with a throttle started,
%% exposes metrics showing the numbers of requests and executions.
%% Those exposed by the master node show the sum of all metrics from all nodes.
%% This allows to quickly see the real rates across the whole system.
%%
%% <h4>Workflow</h4>
%%
%% When a user executes `amoc_throttle:run/2', a request is reported to a metric that runs on the user's node.
%% Then a runner process is spawned on the same node. Its task will be to execute `Fun' asynchronously.
%%
%% A random throttle process which is assigned to the `Name' is asked for
%% permission for asynchronous runner to execute `Fun'.
%% When the request reaches the master node, where the throttle processes reside,
%% the request metric on the master node is updated and the throttle process
%% which got the request starts monitoring the asynchronous runner process.
%%
%% Then, depending on the system's load and the current rate of executions,
%% the asynchronous runner is allowed to run the `Fun' or is compelled to wait,
%% because executing the function would exceed the calculated `Rate' in an `Interval'.
%% When the rate finally allows it, the asynchronous runner gets the permission to run
%% the function from the throttle process.
%%
%% Both processes increase the metrics which count executions, but for each the metric is assigned
%% to their own node. Then the asynchronous runner tries to execute `Fun'.
%% It may succeed or fail, either way it dies and an `EXIT' signal is sent to the throttle process.
%% This way it knows that the execution of a task has ended, and can allow a different process
%% to run its task connected to the same `Name' if the current `Rate' allows it.
%%
%% @end
%%==============================================================================
-module(amoc_throttle).

%% API
-export([start/2,
         start/3,
         start/4,
         send/3,
         send/2,
         send_and_wait/2,
         run/2,
         pause/1,
         resume/1,
         change_rate/3,
         change_rate_gradually/6,
         stop/1]).

-define(DEFAULT_INTERVAL, 60000).%% one minute
-type name() :: atom().

%% @see start/4
-spec start(name(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate) ->
    start(Name, Rate, ?DEFAULT_INTERVAL).

%% @see start/4
-spec start(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval) ->
    start(Name, Rate, Interval, 10).

%% @doc Starts the throttle mechanism for a given `Name' with a given `Rate'.
%%
%% The optional arguments are an `Interval' (default is one minute) and a ` NoOfProcesses' (default is 10).
%% `Name' is needed to identify the rate as a single test can have different rates for different tasks.
%% `Interval' is given in milliseconds and can be changed to a different value for convenience or higher granularity.
%% It also accepts a special value of `0' which limits the number of parallel executions associated with `Name' to `Rate'.
-spec start(name(), pos_integer(), non_neg_integer(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval, NoOfProcesses) ->
    amoc_throttle_controller:ensure_throttle_processes_started(Name, Rate, Interval, NoOfProcesses).

%% @doc Pauses executions for the given `Name' as if `Rate' was set to `0'.
%%
%% Does not stop the scheduled rate changes.
-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    amoc_throttle_controller:pause(Name).

%% @doc Resumes the executions for the given `Name', to their original `Rate' and `Interval' values.
-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    amoc_throttle_controller:resume(Name).

%% @doc Sets `Rate' and `Interval' for `Name' according to the given values.
%%
%% Can change whether Amoc throttle limits `Name' to parallel executions or to `Rate' per `Interval',
%% according to the given `Interval' value.
-spec change_rate(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
change_rate(Name, Rate, Interval) ->
    amoc_throttle_controller:change_rate(Name, Rate, Interval).

%% @doc Allows to set a plan of gradual rate changes for a given `Name'.
%%
%% `Rate' will be changed from `From' to `To' in a series of consecutive steps.
%% `From' does not need to be lower than `To', rates can be changed downwards.
%% The rate is calculated at each step in relation to the `RateInterval', which can also be `0'.
%% Each step will take the `StepInterval' time in milliseconds.
%% There will be `NoOfSteps' steps.
%% Be aware that, at first, the rate will be changed to `From' per `RateInterval' and this is not considered a step.
-spec change_rate_gradually(name(), pos_integer(), pos_integer(),
                            non_neg_integer(), pos_integer(), pos_integer()) ->
    ok | {error, any()}.
change_rate_gradually(Name, LowRate, HighRate, RateInterval, StepInterval, NoOfSteps) ->
    amoc_throttle_controller:change_rate_gradually(Name, LowRate, HighRate, RateInterval,
                                                   StepInterval, NoOfSteps).

%% @doc Executes a given function `Fn' when it does not exceed the rate for `Name'.
%%
%% `Fn' is executed in the context of a new process spawned on the same node on which
%% the process executing `run/2' runs, so a call to `run/2' is non-blocking.
%% This function is used internally by both `send' and `send_and_wait/2' functions,
%% so all those actions will be limited to the same rate when called with the same `Name'.
%%
%% Diagram showing function execution flow in distributed environment,
%% generated using https://sequencediagram.org/:
%% ```
%%        title Amoc distributed
%%        participantgroup  **Slave node**
%%            participant User
%%            participant Async runner
%%        end
%%        participantgroup **Master node**
%%            participant Throttle process
%%        end
%%        box left of User: inc req rate
%%
%%        User -> *Async runner : Fun
%%
%%        User -> Throttle process : {schedule, Async runner PID}
%%        box right of Throttle process : inc req rate
%%
%%        ==throtlling delay==
%%
%%        Throttle process -> Async runner: scheduled
%%
%%        box left of Async runner : inc exec rate
%%        abox over Async runner : Fun()
%%        activate Async runner
%%        box right of Throttle process : inc exec rate
%%        deactivate Async runner
%%        Async runner ->Throttle process:'DOWN'
%%        destroy Async runner
%%  '''
%% for the local execution, req/exec rates are increased only by throttle process.
-spec run(name(), fun(()-> any())) -> ok | {error, any()}.
run(Name, Fn) ->
    amoc_throttle_controller:run(Name, Fn).

%% @doc Sends a given message `Msg' to a given `Pid' when the rate for `Name' allows for that.
%% May be used to schedule tasks.
-spec send(name(), pid(), any()) -> ok | {error, any()}.
send(Name, Pid, Msg) ->
    run(Name, fun() -> Pid ! Msg end).

%% @doc Sends a given message to `erlang:self()'
%% @see send/3
-spec send(name(), any()) -> ok | {error, any()}.
send(Name, Msg) ->
    send(Name, self(), Msg).

%% @doc Sends and receives the given message `Msg'.
%% Can be used to halt execution if we want a process to be idle when waiting for rate increase or other processes finishing their tasks.
-spec send_and_wait(name(), any()) -> ok | {error, any()}.
send_and_wait(Name, Msg) ->
    send(Name, Msg),
    receive
        Msg -> ok
    end.

%% @doc Stops the throttle mechanism for the given `Name'.
-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    amoc_throttle_controller:stop(Name),
    ok.
