%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_local).

-export([do/3,
         add/1,
         remove/2]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
do(Scenario, Start, End) ->
    amoc_controller:do(Scenario, Start, End).

add(Count) ->
    amoc_controller:add(Count).

remove(Count, Opts) ->
    amoc_controller:remove(Count, Opts).
