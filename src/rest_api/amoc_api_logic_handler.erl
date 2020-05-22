%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_logic_handler).

-behaviour(amoc_rest_logic_handler).

-export([handle_request/3]).

-spec handle_request(
    OperationID :: amoc_rest_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.

handle_request('NodesGet', _Req, _Context) ->
    Status = amoc_cluster:get_status(),
    Connected = maps:get(connected, Status, []),
    FailedToConnect = maps:get(failed_to_connect, Status, []),
    ConnectionLost = maps:get(connection_lost, Status, []),
    Up = [{Node, up} || Node <- [node() | Connected]],
    DownNodes = lists:usort(FailedToConnect ++ ConnectionLost),
    Down = [{Node, down} || Node <- DownNodes],
    ResponseList = Up ++ Down,
    {200, #{}, [{<<"nodes">>, ResponseList}]};
handle_request('ScenariosGet', _Req, _Context) ->
    Scenarios = amoc_scenario:list_scenario_modules(),
    BinaryScenarios = [atom_to_binary(S, latin1) || S <- Scenarios],
    {200, #{}, [{<<"scenarios">>, BinaryScenarios}]};
handle_request('StatusGet', _Req, _Context) ->
    Apps = application:which_applications(),
    Status = case lists:keyfind(amoc, 1, Apps) of
                 {amoc, _Desc, _Vsn} -> <<"up">>;
                 false -> <<"down">>
             end,
    {200, #{}, [{<<"node_status">>, Status}]};
handle_request('ScenariosIdGet', _Req, #{id := Resource}) ->
    Scenario = binary_to_atom(Resource, latin1),
    Status = amoc_api_scenario_status:test_status(Scenario),
    BinStatus = atom_to_binary(Status, latin1),
    {200, #{}, [{<<"scenario_status">>, BinStatus}]};
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
        "Got not implemented request to process: ~p~n",
        [{OperationID, Req, Context}]
    ),
    {501, #{}, #{}}.
