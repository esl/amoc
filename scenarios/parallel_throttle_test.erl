%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% @doc
%%   This scenario demonstrates amoc_throttle execution rate limiting
%%   functionality. You might be interested in the next metrics:
%%     - *.amoc.users.size
%%     - *.amoc.throttle.parallel_testing.*.one
%%     - *.amoc.gauge.max_scheduled.value
%%     - *.amoc.gauge.current_scheduled.value
%% @end
%%==============================================================================
-module(parallel_throttle_test).

%% API
-behavior(amoc_scenario).
-export([start/1, init/0]).

-define(PARALLEL_EXECUTION_TEST, parallel_testing).
-define(METRICS_PROC_NAME, max_counter_value).
-define(METRICS_PROC, {?METRICS_PROC_NAME, amoc_cluster:master_node()}).

-spec init() -> ok.
init() ->
    %% not more than 20 simultaneous executions
    amoc_throttle:start(?PARALLEL_EXECUTION_TEST, 20, 0, 3),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(1) ->
    spawn(fun change_throttle_rate/0),
    start_metrics_on_master_node(),
    parallel_execution_scenario();
start(_Id) -> parallel_execution_scenario().

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%
%% user scenario loop %%
%%%%%%%%%%%%%%%%%%%%%%%%
parallel_execution_scenario() ->
    Pid = ?METRICS_PROC,
    Self = self(),
    amoc_throttle:run(?PARALLEL_EXECUTION_TEST,
                      fun() ->
                          Pid ! {delta, 1},
                          timer:sleep(30000),
                          Pid ! {delta, -1},
                          Self ! next
                      end),
    receive next -> ok end,
    parallel_execution_scenario().

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% change throttle rate %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
change_throttle_rate() ->
    %% we start with 20 parallel executions, each execution takes 30 sec.
    %% so it's 40 executions per minute
    timer:sleep(200000),
    %% 0 executions per minute
    amoc_throttle:pause(?PARALLEL_EXECUTION_TEST),
    timer:sleep(150000),
    %% back to 20 parallel executions (40 executions per minute)
    amoc_throttle:resume(?PARALLEL_EXECUTION_TEST),
    timer:sleep(100000),
    %% switch to 20 executions per minute rate
    amoc_throttle:change_rate(?PARALLEL_EXECUTION_TEST, 20, 60000),
    timer:sleep(150000),
    %% 30 parallel executions (60 executions per minute)
    amoc_throttle:change_rate(?PARALLEL_EXECUTION_TEST, 30, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Manual metrics counting on master node %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_metrics_on_master_node() ->
    MasterNode = amoc_cluster:master_node(),
    spawn(MasterNode, fun max_counter_value/0).

max_counter_value() ->
    erlang:register(?METRICS_PROC_NAME, self()),
    amoc_metrics:init(gauge, max_scheduled),
    amoc_metrics:init(gauge, current_scheduled),
    max_counter_value(0, 0).

max_counter_value(Gauge, MaxGauge) when Gauge =:= 0 andalso MaxGauge =/= 0;
                                        Gauge > MaxGauge ->
    amoc_metrics:update_gauge(max_scheduled, Gauge),
    max_counter_value(Gauge, Gauge);
max_counter_value(Gauge, MaxGauge) ->
    receive
        {delta, N} ->
            amoc_metrics:update_gauge(current_scheduled, Gauge + N),
            max_counter_value(Gauge + N, MaxGauge)
    end.
