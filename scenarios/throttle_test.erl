%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(throttle_test).

%% API
-behavior(amoc_scenario).
-export([start/1, init/0]).

-define(RATE_CHANGE_TEST, testing).
-define(PARALLEL_EXECUTION_TEST, parallel_testing).

-spec init() -> ok.
init() ->
    init_metrics(),
    amoc_throttle:start(?RATE_CHANGE_TEST, 20000, 120000, 20), %% 20k per 2 min
    amoc_throttle:start(?PARALLEL_EXECUTION_TEST, 20, 0, 3), %% not more than 20 simultaneous executions
    spawn(
        fun() ->
            timer:sleep(200000),
            amoc_throttle:change_rate_gradually(?RATE_CHANGE_TEST,
                                                1750, 21500, 60000, 200000, 4),
            timer:sleep(950000),
            amoc_throttle:pause(?RATE_CHANGE_TEST),
            timer:sleep(100000),
            amoc_throttle:change_rate_gradually(?RATE_CHANGE_TEST,
                                                20000, 1750, 60000, 200000, 2),
            timer:sleep(100000),
            amoc_throttle:resume(?RATE_CHANGE_TEST),
            timer:sleep(480000), % 8 mins
            amoc_throttle:change_rate(?RATE_CHANGE_TEST, 20019, 60000)
        end),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(1) ->
    MasterNode = amoc_cluster:master_node(),
    Pid = spawn(MasterNode, fun count_per_minute/0),
    rpc:call(MasterNode, erlang, register, [count_per_minute, Pid]),
    parallel_execution_scenario();
start(_Id) -> rate_change_scenario().

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%% metrics initialisation
init_metrics() ->
    amoc_metrics:init(times, execution_delay).

rate_change_scenario() ->
    {TimeDiff, _} = timer:tc(fun rate_change_fn/0),
    {count_per_minute, amoc_cluster:master_node()} ! inc,
    amoc_metrics:update_time(execution_delay, TimeDiff),
    rate_change_scenario().

rate_change_fn() ->
    amoc_throttle:send_and_wait(?RATE_CHANGE_TEST, some_msg).

parallel_execution_scenario() ->
    Pid = spawn(fun max_counter_value/0),
    [parallel_execution_fn(Pid) || _ <- lists:seq(0, 10000)],
    timer:sleep(200000), %% 40 executions per minute
    amoc_throttle:pause(?PARALLEL_EXECUTION_TEST),
    timer:sleep(150000), %% 0 executions per minute
    amoc_throttle:resume(?PARALLEL_EXECUTION_TEST),
    timer:sleep(100000), %% 40 executions per minute
    amoc_throttle:change_rate(?PARALLEL_EXECUTION_TEST, 20, 60000),
    timer:sleep(150000), %% 20 executions per minute
    amoc_throttle:change_rate(?PARALLEL_EXECUTION_TEST, 30, 0),
    timer:sleep(infinity). %% 60 executions per minute, and then 0

parallel_execution_fn(Pid) ->
    %% just sleep 30 seconds
    amoc_throttle:run(?PARALLEL_EXECUTION_TEST,
                      fun() ->
                          Pid ! {delta, 1},
                          timer:sleep(30000),
                          Pid ! {delta, -1}
                      end).

max_counter_value() ->
    amoc_metrics:init(gauge, max_scheduled),
    amoc_metrics:init(gauge, current_scheduled),
    max_counter_value(0, 0).

max_counter_value(Gauge, MaxGauge) when Gauge > MaxGauge ->
    amoc_metrics:update_gauge(max_scheduled, Gauge),
    max_counter_value(Gauge, Gauge);
max_counter_value(Gauge, MaxGauge) ->
    receive
        {delta, N} ->
            amoc_metrics:update_gauge(current_scheduled, Gauge + N),
            max_counter_value(Gauge + N, MaxGauge)
    end.

count_per_minute() ->
    %%this one runs on master amoc node
    amoc_metrics:init(gauge, scheduled_per_minute),
    erlang:send_after(60000, self(), one_minute),
    count_per_minute(0).

count_per_minute(N) ->
    receive
        inc ->
            count_per_minute(N + 1);
        one_minute ->
            erlang:send_after(60000, self(), one_minute),
            amoc_metrics:update_gauge(scheduled_per_minute, N),
            count_per_minute(0)
    end.


