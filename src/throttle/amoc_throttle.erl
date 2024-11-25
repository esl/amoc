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
%% Number of events per given `t:interval/0', or
%% - infinity for effectively unlocking all throttling,
%% - zero for effectively pausing the throttle.

-type interarrival() :: infinity | non_neg_integer().
%% Time in milliseconds between two events, or
%% - infinity for effectively pausing the throttle,
%% - zero for effectively unlocking all throttling.

-type interval() :: pos_integer().
%% In milliseconds, defaults to 60000 (one minute).

-type t() :: #{rate := rate(), interval => interval()} |
             #{interarrival := interarrival()}.
%% Throttle unit of measurement

-type gradual_rate_config() :: #{from_rate := non_neg_integer(),
                                 to_rate := non_neg_integer(),
                                 interval => interval(),
                                 step_interval => pos_integer(),
                                 step_size => pos_integer(),
                                 step_count => pos_integer(),
                                 duration => pos_integer()} |
                               #{from_interarrival := pos_integer(),
                                 to_interarrival := pos_integer(),
                                 step_interval => pos_integer(),
                                 step_size => pos_integer(),
                                 step_count => pos_integer(),
                                 duration => pos_integer()}.
%% Configuration for a gradual throttle rate change
%%
%% `From' and `To' rates are required. `interval' defaults to 1s and `step_size' to 1 (or -1 if applies),
%% that is, the throttle will be changed in increments of 1.
%%
%% All other values can be calculated from the provided.

-export_type([t/0, name/0, rate/0, interval/0, gradual_rate_config/0]).

%% @doc Starts the throttle mechanism for a given `Name' with a given config.
%%
%% `Name' is needed to identify the rate as a single test can have different rates for different tasks.
%% `Interval' is given in milliseconds, the default is one minute,
%% and can be changed to a different value for convenience or higher granularity.
-spec start(name(), t() | rate()) -> {ok, started | already_started} | {error, any()}.
start(Name, #{} = Config) ->
    amoc_throttle_controller:ensure_throttle_processes_started(Name, Config);
start(Name, Rate) ->
    amoc_throttle_controller:ensure_throttle_processes_started(Name, #{rate => Rate}).

%% @doc Pauses executions for the given `Name' as if `Rate' was set to `0'.
%%
%% Does not stop the scheduled rate changes.
-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    amoc_throttle_controller:pause(Name).

%% @doc Unlocks executions for the given `Name' as if `Rate' was set to `infinity'.
-spec unlock(name()) -> ok | {error, any()}.
unlock(Name) ->
    amoc_throttle_controller:unlock(Name).

%% @doc Resumes the executions for the given `Name', to their original `Rate' and `Interval' values.
-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    amoc_throttle_controller:resume(Name).

%% @doc Sets `Throttle' for `Name' according to the given values.
%%
%% Can change whether Amoc throttle limits `Name' to parallel executions or to `Rate' per `Interval',
%% according to the given `Interval' value.
-spec change_rate(name(), t() | rate()) -> ok | {error, any()}.
change_rate(Name, #{} = Config) ->
    amoc_throttle_controller:change_rate(Name, Config);
change_rate(Name, Rate) when is_integer(Rate) ->
    amoc_throttle_controller:change_rate(Name, #{rate => Rate}).

%% @doc Allows to set a plan of gradual rate changes for a given `Name'.
%%
%% `Rate' will be changed from `FromRate' to `ToRate' in a series of consecutive steps.
%% Note that `FromRate' does not need to be lower than `ToRate', rates can be changed downwards.
%%
%% The rate is calculated at each step in relation to the `RateInterval', which can also be `0'.
%% There will be `NoOfSteps' steps, each taking `StepInterval' time in milliseconds.
%%
%% Be aware that, at first, the rate will be changed to `FromRate' per `RateInterval'
%% and this is not considered a step.
-spec change_rate_gradually(name(), gradual_rate_config()) ->
    ok | {error, any()}.
change_rate_gradually(Name, Config) ->
    amoc_throttle_controller:change_rate_gradually(Name, Config).

%% @doc Executes a given function `Fn' when it does not exceed the rate for `Name'.
%%
%% `Fn' is executed in the context of a new process spawned on the same node on which
%% the process executing `run/2' runs, so a call to `run/2' is non-blocking.
%%
%% <a href="https://sequencediagram.org/index.html#initialData=C4S2BsFMAIEEFsD2BjaATEBnYAnEAjAV2EjQCgAHAQx1GRGoDtgBzHRQi6aAKh4GVwVAG4xGiNJD5lu3arRD0mwaAFVMkHDNny6DKsziYAno1Q5CjRprKRG5XYv3M2HLnwCyVbJujjJ0rLQjkoGKgAqABbswBAwFOzIkJiYtvZk+IgAHtBQAGYqiHlqGjgAXNAgZtA4kACONVQkZGTqvgC0AHy8sCbVFla+FQBili1tONBd0FExccGJyZjQFQDemMiRpIRQADRGpuaW1pMACgCSACIAvhnZNSAskYXFs4ixUAsoSyuV-fWNZpkAC8wOA0Xe4HAVRY6EgQmMoJabw+8UWKSm3V6hxqx00FQ2WzQO1ILUyOXyLwO-TxkwqVVQkCykHMTUgZCo5OgiFEk2xNMGdOgo0YAAoAJQc5CgYRs6lHQV3HJ4J5UlHzBLfDH06pMlmA9mSKjSkCykjy3GK-kKk6Y9WfTVJFJlADklwA8gB1AByLrIkmw7GMFoGJzIQA" target="_blank">Diagram</a>
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
%% box left of User: inc req rate
%%
%% User -> *Async runner : Fun
%%
%% User -> Throttle process : {schedule, Async runner PID}
%% box right of Throttle process : inc req rate
%%
%% ==throtlling delay==
%%
%% Throttle process -> Async runner: scheduled
%%
%% box left of Async runner : inc exec rate
%% abox over Async runner : Fun()
%% activate Async runner
%% box right of Throttle process : inc exec rate
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
