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

-define(GROUP_NAME, testing).

init() ->
    init_metrics(),
    amoc_throttle:start(?GROUP_NAME, 20000, 120000, 20), %% 20k per 2 min
    spawn(
        fun() ->
            timer:sleep(200000),
            amoc_throttle:change_rate_gradually(?GROUP_NAME, 1750, 21500, 60000, 200000, 5),
            timer:sleep(100000),
            amoc_throttle:pause(?GROUP_NAME),
            timer:sleep(150000),
            amoc_throttle:resume(?GROUP_NAME),
            timer:sleep(800000),
            amoc_throttle:change_rate_gradually(?GROUP_NAME, 21500, 1750, 50000, 200000, 3)
        end),
    ok.


start(_Id) -> publish_loop().

init_metrics() ->
    amoc_metrics:init(times, publication).

publish_loop() ->
    {TimeDiff, _} = timer:tc(fun publish/0),
    amoc_metrics:update_time(publication, TimeDiff),
    publish_loop().

publish() ->
    amoc_throttle:send_and_wait(?GROUP_NAME, publish).