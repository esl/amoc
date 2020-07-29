%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_scenario_status).
%% API
-export([test_status/1,
         maybe_scenario_settings/2,
         maybe_scenario_params/1,
         get_edoc/1]).

-type status() :: error | running | finished | loaded | doesnt_exist.
-type scenario_status() :: {status(), amoc:scenario()}.

get_edoc(Scenario) ->
    case docsh_lib:get_docs(Scenario) of
        {error, _} ->
            ScenarioName = atom_to_binary(Scenario, utf8),
            <<"cannot extract documentation for ", ScenarioName/binary>>;
        {ok, Docs} ->
            case docsh_format:lookup(Docs, Scenario, [moduledoc]) of
                {not_found, Message} ->
                    <<"no documentation found">>;
                {ok, [DocItem]} ->
                    Doc = maps:get(<<"en">>, DocItem),
                    iolist_to_binary(docsh_edoc:format_edoc(Doc, #{}))
            end
    end.

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

maybe_scenario_params(Scenario)->
    case scenario_parameters(Scenario) of
        []->[];
        Parameters ->
            FormattedParameters = [{format(K), format_kv_list(V)}
                                   || {K, V} <- Parameters],
            [{<<"parameters">>, FormattedParameters}]
    end.

format_kv_list(PropList)->
    [{change_and_format(K), format(V)} || {K, V} <- PropList].

change_and_format(mod)-> format(module);
change_and_format(value)-> format(default_value);
change_and_format(X) -> format(X).

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

scenario_parameters(Scenario) ->
    ConfigMap = amoc_config_scenario:get_default_configuration(Scenario),
    [{Name, maps:to_list(Info)} || {Name, Info} <- maps:to_list(ConfigMap)].

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