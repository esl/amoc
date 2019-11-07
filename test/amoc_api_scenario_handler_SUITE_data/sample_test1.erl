%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(sample_test1).

-behaviour(amoc_scenario).

-include_lib("kernel/include/logger.hrl").

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    ?LOG_INFO("init the scenario"),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    ?LOG_INFO("start user ~p",[Id]),
    amoc_user:stop().