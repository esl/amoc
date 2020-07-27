%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_logic_handler).

-behaviour(amoc_rest_logic_handler).

-include_lib("kernel/include/logger.hrl").

-export([handle_request/3]).

-spec handle_request(OperationID :: amoc_rest_api:operation_id(),
                     Req :: cowboy_req:req(), Context :: #{}) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.
handle_request('NodesGet', _Req, _Context) ->
    Status = amoc_cluster:get_status(),
    Connected = maps:get(connected, Status, []),
    FailedToConnect = maps:get(failed_to_connect, Status, []),
    ConnectionLost = maps:get(connection_lost, Status, []),
    Up = [{Node, <<"up">>} || Node <- [node() | Connected]],
    DownNodes = lists:usort(FailedToConnect ++ ConnectionLost),
    Down = [{Node, <<"down">>} || Node <- DownNodes],
    ResponseList = Up ++ Down,
    {200, #{}, [{<<"nodes">>, ResponseList}]};
handle_request('ScenariosGet', _Req, _Context) ->
    Scenarios = amoc_scenario:list_scenario_modules(),
    BinaryScenarios = [atom_to_binary(S, utf8) || S <- Scenarios],
    {200, #{}, [{<<"scenarios">>, BinaryScenarios}]};
handle_request('StatusGet', _Req, _Context) ->
    Apps = application:which_applications(),
    Status = case lists:keyfind(amoc, 1, Apps) of
                 {amoc, _Desc, _Vsn} -> <<"up">>;
                 false -> <<"down">>
             end,
    {200, #{}, [{<<"node_status">>, Status}]};
handle_request('ScenariosIdGet', _Req, #{id := ScenarioName}) ->
    case amoc_api_scenario_status:test_status(ScenarioName) of
        {doesnt_exist, _} ->
            {404, #{}, #{}};
        {Status, Scenario} ->
            BinStatus = atom_to_binary(Status, utf8),
            MaybeSettings =
                amoc_api_scenario_status:maybe_scenario_settings(Status, Scenario),
            {200, #{}, [{<<"scenario_status">>, BinStatus} | MaybeSettings]}
    end;
handle_request('ScenariosIdPatch', _Req, #{'ScenarioExecution' := Body,
                                           id := ScenarioName}) ->
    Error = {200, #{}, [{<<"scenario">>, <<"error">>}]},
    case amoc_api_scenario_status:test_status(ScenarioName) of
        {doesnt_exist, _} ->
            {404, #{}, #{}};
        {loaded, Scenario} ->
            Users = maps:get(<<"users">>, Body),
            SettingsMap = maps:get(<<"settings">>, Body, #{}),
            Settings = [begin
                            Key = binary_to_atom(K, utf8),
                            {ok, Value} = amoc_config_env:parse_value(V),
                            {Key, Value}
                        end || {K, V} <- maps:to_list(SettingsMap)],
            case amoc_dist:do(Scenario, Users, Settings) of
                {ok, _} -> {200, #{}, [{<<"scenario">>, <<"started">>}]};
                {error, _} -> Error
            end;
        _ ->
            Error
    end;
handle_request('ScenariosUploadPut', Req, _Context) ->
    {ok, ModuleSource, _} = cowboy_req:read_body(Req),
    case amoc_api_upload_scenario:upload(ModuleSource) of
        ok ->
            {200, #{}, [{<<"compile">>, <<"ok">>}]};
        {error, invalid_module} ->
            {400, #{}, [{<<"error">>, <<"invalid module">>}]};
        {error, Error} ->
            {200, #{}, [{<<"compile">>, Error}]}
    end;
handle_request(OperationID, Req, Context) ->
    ?LOG_ERROR("Got not implemented request to process: ~p~n",
               [{OperationID, Req, Context}]),
    {501, #{}, #{}}.
