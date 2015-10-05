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
 -spec do(module(), non_neg_integer(), non_neg_integer()) -> ok | {error, term()}.
do(Scenario, Start, End) ->
    amoc_controller:do(Scenario, Start, End).

-spec add(non_neg_integer()) -> ok.
add(Count) ->
    amoc_controller:add(Count).

-spec remove(non_neg_integer(), list(term())) -> ok.
remove(Count, Opts) ->
    amoc_controller:remove(Count, Opts).
