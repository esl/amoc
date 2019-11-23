%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_dist).

-export([do/3,
         do/4,
         add/1,
         add/2,
         remove/2,
         remove/3,
         amoc_nodes/0,
         test_status/1]).

-compile({no_auto_import, [ceil/1]}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec test_status(amoc:scenario()) -> amoc_controller:scenario_status().
test_status(ScenarioName) ->
    Hosts = [erlang:node() | amoc_nodes()],
    Status = [get_node_test_status(ScenarioName, Host) || Host <- Hosts],
    pick_status(Status, [error, loaded, running, finished]).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id()) ->
    [any()].
do(Scenario, Start, End) ->
    do(Scenario, Start, End, []).

-spec do(amoc:scenario(), amoc_scenario:user_id(),
         amoc_scenario:user_id(), amoc:do_opts()) -> [any()].
do(Scenario, Start, End, Opts) ->
    Nodes = proplists:get_value(nodes, Opts, amoc_nodes()),
    [amoc_slave:set_master_node(Node) || Node <- [node() | Nodes]],
    Count = length(Nodes),
    [amoc_controller:do(Node, Scenario, Start, End, Count, Id, Opts) ||
        {Id, Node} <- lists:zip(lists:seq(1, Count), Nodes)].

-spec add(non_neg_integer()) -> [ok].
add(Count) ->
    add(Count, amoc_nodes()).

-spec add(non_neg_integer(), [node()]) -> [ok].
add(Count, Nodes) ->
    [amoc_controller:add(Node, Count) || Node <- Nodes].

-spec remove(non_neg_integer(), amoc:remove_opts()) -> [ok].
remove(Count, Opts) ->
    remove(Count, Opts, amoc_nodes()).

-spec remove(non_neg_integer(), amoc:remove_opts(), [node()]) -> [ok].
remove(Count, Opts, Nodes) ->
    CountPerNode = ceil(Count / length(Nodes)),
    [amoc_controller:remove(Node, CountPerNode, Opts) || Node <- Nodes].

-spec amoc_nodes() -> [node()].
amoc_nodes() ->
    Status = amoc_slave:get_status(),
    maps:get(connected, Status, []).

%% ------------------------------------------------------------------
%% Local functions
%% ------------------------------------------------------------------
-spec ceil(float()) -> integer().
ceil(Number) ->
    erlang:round(Number + 0.5).

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
