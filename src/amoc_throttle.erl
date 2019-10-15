%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
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

-spec start(name(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate) ->
    start(Name, Rate, ?DEFAULT_INTERVAL).

-spec start(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval) ->
    start(Name, Rate, Interval, 10).

-spec start(name(), pos_integer(), non_neg_integer(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval, NoOfProcesses) ->
    amoc_throttle_controller:ensure_throttle_processes_started(Name, Rate, Interval, NoOfProcesses).

-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    amoc_throttle_controller:pause(Name).

-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    amoc_throttle_controller:resume(Name).

-spec change_rate(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
change_rate(Name, Rate, Interval) ->
    amoc_throttle_controller:change_rate(Name, Rate, Interval).

-spec change_rate_gradually(name(), pos_integer(), pos_integer(),
                            non_neg_integer(), pos_integer(), pos_integer()) -> ok | {error, any()}.
change_rate_gradually(Name, LowRate, HighRate, RateInterval, StepInterval, NoOfSteps) ->
    amoc_throttle_controller:change_rate_gradually(Name, LowRate, HighRate, RateInterval, StepInterval, NoOfSteps).

-spec run(name(), fun(()-> any())) -> ok | {error, any()}.
run(Name, Fn) ->

    %% Diagram showing function execution flow in distributed env.
    %% generated using https://sequencediagram.org/ (remove "%%"):
    %%
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
    %%
    %% for the local execution, req/exec rates are increased only by throttle process.


    amoc_throttle_controller:run(Name, Fn).

-spec send(name(), pid(), any()) -> ok | {error, any()}.
send(Name, Pid, Msg) ->
    run(Name, fun() -> Pid ! Msg end).

-spec send(name(), any()) -> ok | {error, any()}.
send(Name, Msg) ->
    send(Name, self(), Msg).

-spec send_and_wait(name(), any()) -> ok | {error, any()}.
send_and_wait(Name, Msg) ->
    send(Name, Msg),
    receive
        Msg -> ok
    end.

-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    amoc_throttle_controller:stop(Name),
    ok.