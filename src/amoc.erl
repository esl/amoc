%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc).

-export([do/3,
         add/1,
         remove/2]).

-export_type([
              scenario/0,
              do_opts/0,
              remove_opts/0
             ]).

-type scenario() :: module().

-type do_opt() :: {nodes, [node()]} | {comment, string()} | {repeat, timeout()}
                  | {interarrival, timeout()}.
-type do_opts() :: [do_opt()].

-type remove_opt() :: {force, boolean()}.
-type remove_opts() :: [remove_opt()].

%% ------------------------------------------------------------------
%% API for local scenario execution, use amoc_dist module to run
%% scenarios in a distributed environment
%% ------------------------------------------------------------------
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