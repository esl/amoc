%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_scenario_status).
%% API
-export([test_status/1,
         maybe_scenario_settings/2]).

-type status() :: error | running | finished | loaded | doesnt_exist.
-type scenario_status() :: {status(), amoc:scenario()}.

-spec test_status(binary()) -> scenario_status().
test_status(ScenarioName) ->
    case get_scenario(ScenarioName) of
        {ok, Scenario} ->
            Nodes = amoc_cluster:all_nodes(),
            Statuses = [get_node_test_status(Scenario, Node) || Node <- Nodes],
            StatusPriority = [doesnt_exist, error, loaded, running, finished],
            Status = pick_status(Statuses, StatusPriority),
            {Status, Scenario};
        doesnt_exist ->
            {doesnt_exist, invalid_scenario_name}
    end.

maybe_scenario_settings(Status, Scenario)->
    case scenario_settings(Status, Scenario) of
        []->[];
        Settings ->
            FormattedSettings = [{format(K), format(V)} || {K, V} <- Settings],
            [{<<"settings">>, FormattedSettings}]
    end.

format(Value) ->
    list_to_binary(lists:flatten(io_lib:format("~tp", [Value]))).

-spec scenario_settings(status(), amoc:scenario()) -> amoc_config:settings().
scenario_settings(loaded, Scenario) ->
    ConfigMap = amoc_config_scenario:get_default_configuration(Scenario),
    [{Name, Value} || {Name, #{value := Value}} <- maps:to_list(ConfigMap)];
scenario_settings(running, _Scenario) ->
    ConfigMap = amoc_config_scenario:get_current_configuration(),
    [{Name, Value} || {Name, #{value := Value}} <- maps:to_list(ConfigMap)];
scenario_settings(_, _Scenario) -> [].

-spec get_scenario(binary()) -> {ok, amoc:scenario()} | doesnt_exist.
get_scenario(ScenarioName) ->
    try
        {ok, binary_to_existing_atom(ScenarioName, utf8)}
    catch
        error:badarg -> doesnt_exist
    end.

-spec get_node_test_status(amoc:scenario(), atom()) -> disabled | status().
get_node_test_status(Scenario, Node) ->
    try
        case rpc:call(Node, amoc_controller, get_status, []) of
            {idle, Scenarios} ->
                case lists:member(Scenario, Scenarios) of
                    true -> loaded;
                    false -> doesnt_exist
                end;
            {running, Scenario, _, _} -> running;
            {finished, Scenario} -> finished;
            {error, _} -> error;
            disabled -> disabled;
            {badrpc, _} -> error
        end
    catch _:_ ->
        error
    end.

-spec pick_status([disabled | status()], [status()]) -> status().
pick_status(StatusList, [H | T]) ->
    case lists:member(H, StatusList) of
        true -> H;
        false -> pick_status(StatusList, T)
    end.