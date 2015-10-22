%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_dist).

-export([start_nodes/0,
         do/3,
         do/4,
         add/1,
         add/2,
         remove/2,
         remove/3]).
%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec start_nodes() -> [ok].
start_nodes() ->
    Hosts = application:get_env(amoc, hosts, []),
    Path = application:get_env(amoc, path, "/usr"),
    start_nodes(Hosts, Path).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id()) ->
    [any()].
do(Scenario, Start, End) ->
    do(Scenario, Start, End, []).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id(),
         amoc:do_opts()) -> [any()].
do(Scenario, Start, End, Opts) ->
    Nodes = proplists:get_value(nodes, Opts, nodes()),

    _InterArrival = proplists:get_value(interarrival, Opts, 75),
    _RepeatTimeout = proplists:get_value(repeat, Opts, 75),
    _Step = proplists:get_value(step, Opts, 1),

    amoc_event:notify({dist_do, Scenario, Start, End, Opts}),
    Count = length(Nodes),
    [ amoc_controller:do(Node, Scenario, Start, End, Count, Id, Opts) ||
      {Id, Node} <- lists:zip(lists:seq(1, Count), Nodes) ].

-spec add(non_neg_integer()) -> [ok].
add(Count) ->
    add(Count, nodes()).

-spec add(non_neg_integer(), [node()]) -> [ok].
add(Count, Nodes) ->
    amoc_event:notify({dist_add, Count}),
    [ amoc_controller:add(Node, Count) || Node <- Nodes ].

-spec remove(non_neg_integer(), amoc:remove_opts()) -> [ok].
remove(Count, Opts) ->
    remove(Count, Opts, nodes()).

-spec remove(non_neg_integer(), amoc:remove_opts(), [node()]) -> [ok].
remove(Count, Opts, Nodes) ->
    amoc_event:notify({dist_remove, Count, Opts}),
    CountPerNode = ceil(Count / length(Nodes)),
    [ amoc_controller:remove(Node, CountPerNode, Opts) || Node <- Nodes ].

%% ------------------------------------------------------------------
%% Local functions
%% ------------------------------------------------------------------
-spec start_nodes([string()], file:filename()) -> [ok].
start_nodes(Hosts, Path) ->
    [ amoc_slave:start(Host, Path) || Host <- Hosts ].

-spec ceil(float()) -> integer().
ceil(Number) ->
    erlang:round(Number+0.5).
