%% @copyright 2024 Erlang Solutions Ltd.
%% @doc This module allows to synchronize the users and act on groups of them.
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

-define(DEFAULT_NO_PROCESSES, 10).
-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(NONNEG_INT(N), (is_integer(N) andalso N >= 0)).
-define(POS_INT(N), (is_integer(N) andalso N > 0)).

-type name() :: atom().
-type rate() :: pos_integer().
-type interval() :: non_neg_integer().
%% In milliseconds, defaults to 60000 (one minute) when not given.
%% An interval of 0 means no delay at all, only the number of simultaneous executions will be
%% controlled, which corresponds to the number of processes started
-export_type([name/0, rate/0, interval/0]).

%% @see start/4
-spec start(name(), rate()) -> ok | {error, any()}.
start(Name, Rate) ->
    start(Name, Rate, ?DEFAULT_INTERVAL).

%% @see start/4
-spec start(name(), rate(), non_neg_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval) ->
    start(Name, Rate, Interval, ?DEFAULT_NO_PROCESSES).

%% @doc Starts the throttle mechanism for a given `Name' with a given `Rate' per `Interval'.
%%
%% The optional arguments are an `Interval' (default is one minute) and a ` NoOfProcesses' (default is 10).
%% `Name' is needed to identify the rate as a single test can have different rates for different tasks.
%% `Interval' is given in milliseconds and can be changed to a different value for convenience or higher granularity.
%% It also accepts a special value of `0' which limits the number of parallel executions associated with `Name' to `Rate'.
-spec start(name(), rate(), interval(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval, NoOfProcesses)
  when is_atom(Name), ?POS_INT(Rate), ?NONNEG_INT(Interval), ?POS_INT(NoOfProcesses) ->
    amoc_throttle_controller:ensure_throttle_processes_started(Name, Rate, Interval, NoOfProcesses);
start(_Name, _Rate, _Interval, _NoOfProcesses) ->
    {error, invalid_throttle}.

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
-spec change_rate(name(), rate(), interval()) -> ok | {error, any()}.
change_rate(Name, Rate, Interval) ->
    amoc_throttle_controller:change_rate(Name, Rate, Interval).

%% @doc Allows to set a plan of gradual rate changes for a given `Name'.
%%
%% `Rate' will be changed from `FromRate' to `ToRate' in a series of consecutive steps.
%% Note that `FromRate' does not need to be lower than `ToRate', rates can be changed downwards.
%%
%% The rate is calculated at each step in relation to the `RateInterval', which can also be `0'.
%% There will be `NoOfSteps' steps, each taking `StepInterval' time in milliseconds.
%%
%% Be aware that, at first, the rate will be changed to `FromRate' per `RateInterval' and this is not considered a step.
-spec change_rate_gradually(name(), rate(), rate(), interval(), pos_integer(), pos_integer()) ->
    ok | {error, any()}.
change_rate_gradually(Name, FromRate, ToRate, RateInterval, StepInterval, NoOfSteps) ->
    amoc_throttle_controller:change_rate_gradually(
      Name, FromRate, ToRate, RateInterval, StepInterval, NoOfSteps).

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
-spec run(name(), fun(() -> any())) -> ok | {error, any()}.
run(Name, Fn) ->
    amoc_throttle_controller:run(Name, Fn).

%% @doc Sends a given message `Msg' to a given `Pid' when the rate for `Name' allows for that.
%%
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
    case send(Name, Msg) of
        ok ->
            receive
                Msg -> ok
            end;
        Error ->
            Error
    end.

%% @doc Stops the throttle mechanism for the given `Name'.
-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    amoc_throttle_controller:stop(Name),
    ok.
