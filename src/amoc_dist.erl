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
    Hosts = amoc_config:get(hosts, []),
    Path = amoc_config:get(path, "/usr"),
    start_nodes(Hosts, Path).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id()) ->
    [any()].
do(Scenario, Start, End) ->
    do(Scenario, Start, End, []).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id(),
         amoc:do_opts()) -> [any()].
do(Scenario, Start, End, Opts) ->
    Nodes = proplists:get_value(nodes, Opts, amoc_nodes()),

    _InterArrival = proplists:get_value(interarrival, Opts, 75),
    _RepeatTimeout = proplists:get_value(repeat, Opts, 75),
    _Step = proplists:get_value(step, Opts, 1),

    amoc_event:notify({dist_do, Scenario, Start, End, Opts}),
    Count = length(Nodes),
    [ amoc_controller:do(Node, Scenario, Start, End, Count, Id, Opts) ||
      {Id, Node} <- lists:zip(lists:seq(1, Count), Nodes) ].

-spec add(non_neg_integer()) -> [ok].
add(Count) ->
    add(Count, amoc_nodes()).

-spec add(non_neg_integer(), [node()]) -> [ok].
add(Count, Nodes) ->
    amoc_event:notify({dist_add, Count}),
    [ amoc_controller:add(Node, Count) || Node <- Nodes ].

-spec remove(non_neg_integer(), amoc:remove_opts()) -> [ok].
remove(Count, Opts) ->
    remove(Count, Opts, amoc_nodes()).

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

-spec amoc_nodes() -> [node()].
amoc_nodes() ->
    [ Node || Node <- erlang:nodes(), not is_remsh_node(Node) ].

-spec is_remsh_node(node()) -> boolean().
is_remsh_node(Node) ->
    case atom_to_list(Node) of
        "remsh" ++ _ -> true;
        _            -> false
    end.

-spec ceil(float()) -> integer().
ceil(Number) ->
    erlang:round(Number+0.5).

%% ------------------------------------------------------------------
%% Unit tests
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_remsh_node_test_() ->
    [
     ?_assertEqual(false, is_remsh_node('amoc@localhost')),
     ?_assertEqual(false, is_remsh_node('amoc_master@10.100.0.70')),
     ?_assertEqual(true, is_remsh_node('remsh46465bec-amoc_master@10.100.0.70'))
    ].
-endif.
