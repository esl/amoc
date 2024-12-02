%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Allows limiting the number of users' actions per interval.
-module(amoc_throttle).

%% API
-export([start/2, stop/1,
         send/2, send/3, wait/1,
         run/2, pause/1, resume/1, unlock/1,
         change_rate/2, change_rate_gradually/2]).

-type name() :: atom().
%% Atom representing the name of the throttle.

-type rate() :: infinity | non_neg_integer().
%% Number of events per given `t:interval/0'.
%%
%% It can also be:
%% <ul>
%%   <li>infinity for effectively unlocking all throttling,</li>
%%   <li>zero for effectively pausing the throttle.</li>
%% </ul>

-type interarrival() :: infinity | non_neg_integer().
%% Time in milliseconds between two events.
%%
%% It can also be:
%% <ul>
%%   <li>infinity for effectively pausing the throttle,</li>
%%   <li>zero for effectively unlocking all throttling.</li>
%% </ul>

-type interval() :: non_neg_integer().
%% In milliseconds, defaults to 60000 (one minute).
%%
%% Note that an interval of zero means effectively allowing `t:rate/0' number of executions in
%% parallel. It might be expected for this to be always `infinity' as a result of the limit when
%% dividing by zero, but this needs to be made explicit in the `t:rate/0' by setting it to infinity.

-type t() :: #{rate := rate(), interval => interval()} |
             #{interarrival := interarrival()}.
%% Throttle unit of measurement

-type gradual() :: #{from_rate := non_neg_integer(),
                     to_rate := non_neg_integer(),
                     interval => interval()} |
                   #{from_interarrival := non_neg_integer(),
                     to_interarrival := non_neg_integer()}.
%% Configuration throttle for a gradual rate change.
%%
%% "from" and "to" prefixed parameters, whether rates or interarrivals, are required.
%% `interval' applies only to rate and defaults to 1s.

-type plan() :: #{step_interval := pos_integer(),
                  step_count := pos_integer()} |
                #{duration := pos_integer()}.
%% Configuration plan for a gradual rate change.
%%
%% The throttle mechanism will take a series of discrete steps,
%% for as long as the duration given,
%% or in the shape of the `step_interval' and `step_count'.

-type gradual_plan() :: #{throttle := gradual(),
                          plan := plan()}.
%% Gradual plan details. Must specify a `t:gradual/0', and a `t:plan/0'.

-export_type([t/0, name/0, rate/0, interval/0, interarrival/0, gradual_plan/0]).

%% @doc Starts the throttle mechanism for a given `Name' with a given config.
%%
%% `Name' is needed to identify the rate as a single test can have different rates for different tasks.
-spec start(name(), t() | rate()) -> {ok, started | already_started} | {error, any()}.
start(Name, #{} = Config) ->
    case amoc_throttle_config:verify_config(Config) of
        {error, Error} ->
            {error, Error};
        VerifiedConfig ->
            amoc_throttle_controller:ensure_throttle_processes_started(Name, VerifiedConfig)
    end;
start(Name, Rate) when is_integer(Rate) ->
    start(Name, #{rate => Rate}).

%% @doc Pauses executions for the given `Name' as if `Rate' was set to `0'.
%%
%% Does not stop the scheduled rate changes. `resume/1' undoes the pausing.
-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    amoc_throttle_controller:pause(Name).

%% @doc Resumes the executions for the given `Name', to their original configuration value.
%%
%% It is the counterpart to the `pause/1' API, resuming the execution of what that mechanism paused.
-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    amoc_throttle_controller:resume(Name).

%% @doc Unlocks executions for the given `Name' by setting `Rate' to `infinity'.
-spec unlock(name()) -> ok | {error, any()}.
unlock(Name) ->
    change_rate(Name, #{rate => infinity, interval => 0}).

%% @doc Sets the throttle `Config' for `Name' according to the given values.
-spec change_rate(name(), t() | rate()) -> ok | {error, any()}.
change_rate(Name, #{} = Config) ->
    case amoc_throttle_config:verify_config(Config) of
        {error, Error} ->
            {error, Error};
        VerifiedConfig ->
            amoc_throttle_controller:change_rate(Name, VerifiedConfig)
    end;
change_rate(Name, Rate) when is_integer(Rate) ->
    change_rate(Name, #{rate => Rate}).

%% @doc Allows to set a plan of gradual rate changes for a given `Name'.
%%
%% The configuration will be changed in a series of consecutive steps.
%% Rates can be changed upwards as well as downwards.
%% See the documentation for `t:gradual_plan/0' for more info.
%%
%% Be aware that, at first, the rate will be changed to the initial point given
%% in the configuration, and this is not considered a step.
-spec change_rate_gradually(name(), gradual_plan()) ->
    ok | {error, any()}.
change_rate_gradually(Name, Config) ->
    case amoc_throttle_config:verify_gradual_config(Config) of
        {error, _} = Error ->
            Error;
        VerifiedConfig ->
            amoc_throttle_controller:change_rate_gradually(Name, VerifiedConfig)
    end.

%% @doc Executes a given function `Fn' when it does not exceed the rate for `Name'.
%%
%% `Fn' is executed in the context of a new process spawned on the same node on which
%% the process executing `run/2' runs, so a call to `run/2' is non-blocking.
%%
%% <a href="https://sequencediagram.org/index.html#initialData=C4S2BsFMAIEEFsD2BjaATEBnYAnEAjAV2EjQCgyAHAQx1GRBoDtgBzHRQy6aAKl4DK4agDcYTRGkj8yPHjTogGzYNACqmSDllyF9RtRZxMATyaochJky1lITcnqUGW7Tt34BZati3QJUjJy0E7KhqoAKgAWHMAQMJQcyJCYmHYOZPiIAB7QUABmqoj56po4AFzQOJAAjoQpqiRQ8JC4JtCQYiwUGn4AtAB8fLCm5lVWNjjQlQBiVj1l0IPQ0bHxIUkpmNPQAN6YyFGkhFAANMZmFhN+AAoAkgAiAL6ZOVUgrFFFJauIcVAbFBbHbVOoNaBNSAtNodLrACgAXgRwBif3A4BATFY6EgwhMSIov3+CU2qSWQxGl3G1i0lQORzQJ1Ir1yeE+3xWqOJgOSZMqkGykGQxBAiCYENxUNaOHanXs8MwNGSSwArCy8pBCtBihcxpYaVN+YLhaAxRLmtLZXCyNQsrlEGIppS9ddDdB6ccoGhoNRkKamDa-SARNQSLqrgayIrfZAyFJfaAQ2HnRHJmQU9TJuSietEkDUuUAOQPADyAHUAHKFuMNDjtDP6yZAA" target="_blank">Diagram</a>
%% showing function execution flow in distributed environment.
%% ```
%% title Amoc distributed
%% participantgroup  **Slave node**
%%     participant User
%%     participant Async runner
%% end
%% participantgroup **Master node**
%%     participant Throttle process
%% end
%% box left of User: request telemetry event
%%
%% User -> *Async runner : Fun
%%
%% User -> Throttle process : {schedule, Async runner PID}
%% box right of Throttle process : request telemetry event
%%
%% ==throtlling delay==
%%
%% Throttle process -> Async runner: scheduled
%% box right of Throttle process : execution telemetry event
%% space -5
%% box left of Async runner : execution telemetry event
%% abox over Async runner : scheduled action
%% activate Async runner
%% space
%% deactivate Async runner
%% Async runner ->Throttle process:'DOWN'
%% destroy Async runner
%% '''
-spec run(name(), fun(() -> any())) -> ok | {error, any()}.
run(Name, Fn) ->
    amoc_throttle_runner:throttle(Name, Fn).

%% @see send/3
%% @doc Sends a given message to `erlang:self()'
-spec send(name(), any()) -> ok | {error, any()}.
send(Name, Msg) ->
    amoc_throttle_runner:throttle(Name, {self(), Msg}).

%% @doc Sends a given message `Msg' to a given `Pid' when the rate for `Name' allows for that.
%%
%% May be used to schedule tasks.
-spec send(name(), pid(), any()) -> ok | {error, any()}.
send(Name, Pid, Msg) ->
    amoc_throttle_runner:throttle(Name, {Pid, Msg}).

%% @doc Blocks the caller until the throttle mechanism allows.
-spec wait(name()) -> ok | {error, any()}.
wait(Name) ->
    amoc_throttle_runner:throttle(Name, wait).

%% @doc Stops the throttle mechanism for the given `Name'.
-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    amoc_throttle_controller:stop(Name).
