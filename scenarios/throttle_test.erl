%%%-------------------------------------------------------------------
%%% @author denys
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%   simple scenario for throttle helper manual testing
%%% @end
%%% Created : 29. Mar 2019 17:09
%%%-------------------------------------------------------------------
-module(throttle_test).
-author("denys").

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
                                                1750, 21500, 60000, 200000, 5),
            timer:sleep(950000),
            amoc_throttle:pause(?RATE_CHANGE_TEST),
            timer:sleep(100000),
            amoc_throttle:change_rate_gradually(?RATE_CHANGE_TEST,
                                                20000, 1750, 60000, 200000, 3),
            timer:sleep(100000),
            amoc_throttle:resume(?RATE_CHANGE_TEST)
        end),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(1) -> parallel_execution_scenario();
start(_Id) -> rate_change_scenario().

init_metrics() ->
    amoc_metrics:init(times, execution_delay).

rate_change_scenario() ->
    {TimeDiff, _} = timer:tc(fun rate_change_fn/0),
    amoc_metrics:update_time(execution_delay, TimeDiff),
    rate_change_scenario().

rate_change_fn() ->
    amoc_throttle:send_and_wait(?RATE_CHANGE_TEST, some_msg).

parallel_execution_scenario() ->
    [parallel_execution_fn() || _ <- lists:seq(0, 10000)],
    timer:sleep(200000),
    amoc_throttle:pause(?PARALLEL_EXECUTION_TEST),
    timer:sleep(150000),
    amoc_throttle:resume(?PARALLEL_EXECUTION_TEST),
    timer:sleep(100000),
    amoc_throttle:change_rate(?PARALLEL_EXECUTION_TEST, 20, 60000),
    timer:sleep(150000),
    amoc_throttle:change_rate(?PARALLEL_EXECUTION_TEST, 30, 0),
    timer:sleep(infinity).

parallel_execution_fn() ->
    %% just sleep 30 seconds
    amoc_throttle:run(?PARALLEL_EXECUTION_TEST, fun() -> timer:sleep(30000) end).
