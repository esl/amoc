%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_scenario_status).
%% API
-export([test_status/1]).

-type scenario_status() :: error | running | finished | loaded.

-spec test_status(amoc:scenario()) -> scenario_status().
test_status(ScenarioName) ->
    Nodes = amoc_cluster:all_nodes(),
    Status = [get_node_test_status(ScenarioName, Node) || Node <- Nodes],
    pick_status(Status, [error, loaded, running, finished]).

-spec get_node_test_status(amoc:scenario(), atom()) -> disabled | scenario_status().
get_node_test_status(ScenarioName, Node) ->
    try
        case rpc:call(Node, amoc_controller, get_status, []) of
            {idle, Scenarios} ->
                case lists:member(ScenarioName, Scenarios) of
                    true -> loaded;
                    false -> error
                end;
            {running, ScenarioName, _, _} -> running;
            {finished, ScenarioName} -> finished;
            {error, _} -> error;
            disabled -> disabled;
            {badrpc, _} -> error
        end
    catch _:_ ->
        error
    end.

-spec pick_status([scenario_status()], [scenario_status()]) -> scenario_status().
pick_status(StatusList, [H | T]) ->
    case lists:member(H, StatusList) of
        true -> H;
        false -> pick_status(StatusList, T)
    end.