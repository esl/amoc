%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_dist).

-export([gather_nodes/0,
         ping_nodes/0,
         do/3,
         do/4,
         add/1,
         add/2,
         remove/2,
         remove/3,
         test_status/1]).

-compile({no_auto_import,[ceil/1]}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec gather_nodes() -> [ok].
gather_nodes() ->
    Hosts = amoc_config:get(hosts, []),
    gather_nodes(Hosts).

-spec ping_nodes() -> [{atom(), pong|pang}].
ping_nodes() ->
    Hosts = amoc_config:get(hosts, []),
    [ {erlang:list_to_atom(Host), ping_node(amoc_slave:node_name(Host))} 
        || Host <- Hosts ].

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id()) ->
    [any()].
do(Scenario, Start, End) ->
    do(Scenario, Start, End, []).

-spec test_status(amoc:scenario()) -> amoc_controller:scenario_status().
test_status(ScenarioName) ->
    Hosts = [erlang:node() | erlang:nodes()],
    Status = [get_node_test_status(ScenarioName, Host) || Host <- Hosts],
    pick_status(Status, [error, loaded, running, finished]).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id(),
         amoc:do_opts()) -> [any()].
do(Scenario, Start, End, Opts) ->
    Nodes = proplists:get_value(nodes, Opts, amoc_nodes()),
    [amoc_slave:monitor_master(Node) || Node <- [node() | Nodes]],

    _InterArrival = proplists:get_value(interarrival, Opts, 75),
    _RepeatTimeout = proplists:get_value(repeat, Opts, 75),
    _Step = proplists:get_value(step, Opts, 1),

    amoc_controller:start_scenario_checking(Scenario),
    Count = length(Nodes),
    [ amoc_controller:do(Node, Scenario, Start, End, Count, Id, Opts) ||
      {Id, Node} <- lists:zip(lists:seq(1, Count), Nodes) ].

-spec add(non_neg_integer()) -> [ok].
add(Count) ->
    add(Count, amoc_nodes()).

-spec add(non_neg_integer(), [node()]) -> [ok].
add(Count, Nodes) ->
    [ amoc_controller:add(Node, Count) || Node <- Nodes ].

-spec remove(non_neg_integer(), amoc:remove_opts()) -> [ok].
remove(Count, Opts) ->
    remove(Count, Opts, amoc_nodes()).

-spec remove(non_neg_integer(), amoc:remove_opts(), [node()]) -> [ok].
remove(Count, Opts, Nodes) ->
    CountPerNode = ceil(Count / length(Nodes)),
    [ amoc_controller:remove(Node, CountPerNode, Opts) || Node <- Nodes ].

%% ------------------------------------------------------------------
%% Local functions
%% ------------------------------------------------------------------
-spec gather_nodes([string()]) -> [ok].
gather_nodes(Hosts) ->
    [ amoc_slave:gather_node(Host) || Host <- Hosts ].

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

-spec ping_node(node()) -> pong|pang.
ping_node(Node) ->
    case amoc_slave:ping(Node) of
        pong ->
              pong;
        pang ->
              pang
    end.

-spec pick_status([amoc_controller:scenario_status()], 
                  [amoc_controller:scenario_status()]) ->
                  amoc_controller:scenario_status().
pick_status(StatusList, [H | T]) ->
    case lists:member(H, StatusList) of
        true -> H;
        false -> pick_status(StatusList, T)
    end.

-spec get_node_test_status(amoc:scenario(), atom()) -> 
            amoc_controller:scenario_status().
get_node_test_status(ScenarioName, Node) ->    
    try gen_server:call({amoc_controller, Node}, {status, ScenarioName}) of
        Res -> Res
    catch _:_ ->
        error
    end.

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
