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
%% ----------------------------------------------------------------
 -spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id()) ->
     ok | {error, term()}.
do(Scenario, Start, End) ->
    amoc_controller:do(Scenario, Start, End).

-spec add(non_neg_integer()) -> ok.
add(Count) ->
    amoc_controller:add(Count).

-spec remove(non_neg_integer(), amoc:remove_opts()) -> ok.
remove(Count, Opts) ->
    amoc_controller:remove(Count, Opts).
