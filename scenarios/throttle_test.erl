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

-define(GROUP_NAME, throttle_testing).

init() ->
    init_metrics(),
    throttle:start(?GROUP_NAME, 20000, 120000, 20). %% 20k per 2 min

start(_Id) -> publish_loop().

init_metrics() ->
    amoc_metrics:init(counters, publications),
    amoc_metrics:init(times, publication).

publish_loop() ->
    Self = self(), Time = os:system_time(microsecond),
    throttle:run(?GROUP_NAME,
                 fun() ->
                     publish(),
                     Self ! published
                 end),
    receive
        published ->
            TimeDiff=os:system_time(microsecond) -Time,
            amoc_metrics:update_time(publication, TimeDiff)
    end,
    publish_loop().

publish() ->
    amoc_metrics:update_counter(publications, 1).
