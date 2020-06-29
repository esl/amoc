%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% @doc
%%   This scenario demonstrates amoc_throttle execution rate limiting
%%   functionality. You might be interested in the next metrics:
%%     - *.amoc.users.size
%%     - *.amoc.throttle.testing.rate.value
%%     - *.amoc.throttle.testing.*.one
%%     - *.amoc.gauge.scheduled_per_minute.value
%% @end
%%==============================================================================
-module(rate_throttle_test).

%% API
-behavior(amoc_scenario).
-export([start/1, init/0]).

-define(RATE_CHANGE_TEST, testing).
-define(METRICS_PROC_NAME, count_per_minute).
-define(METRICS_PROC, {?METRICS_PROC_NAME, amoc_cluster:master_node()}).

-spec init() -> ok.
init() ->
    amoc_throttle:start(?RATE_CHANGE_TEST, 20000, 120000, 20), %% 20k per 2 min
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(1) ->
    spawn(fun change_throttle_rate/0),
    start_metrics_on_master_node(),
    rate_change_scenario();
start(_Id) -> rate_change_scenario().

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%
%% user scenario loop %%
%%%%%%%%%%%%%%%%%%%%%%%%
rate_change_scenario() ->
    amoc_throttle:send_and_wait(?RATE_CHANGE_TEST, some_msg),
    ?METRICS_PROC ! inc,
    rate_change_scenario().

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% change throttle rate %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
change_throttle_rate() ->
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
    amoc_throttle:change_rate(?RATE_CHANGE_TEST, 20019, 60000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Manual metrics counting on master node %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_metrics_on_master_node() ->
    MasterNode = amoc_cluster:master_node(),
    spawn(MasterNode, fun count_per_minute/0).

count_per_minute() ->
    %% !!! this code runs only on the master amoc node !!!
    %% sometimes exometer shows execution rate higher than
    %% expected, however that is not true. it's caused by
    %% the way how exometer internally calculates spiral
    %% metrics. below is the simple algorithm for manual
    %% spiral metric calculation, use 'scheduled_per_minute'
    %% metric to ensure that amoc_throttle is not crossing
    %% the upper execution rate boundary
    erlang:register(?METRICS_PROC_NAME, self()),
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
