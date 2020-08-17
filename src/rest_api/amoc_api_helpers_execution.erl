%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_helpers_execution).

%% API
-export([start/1, stop/0, add_users/1, remove_users/1, update_settings/1]).

-type body() :: #{binary() => any()}.
-type ret_value() :: {ok, any} | {error, any()}.

-spec start(body()) -> ret_value().
start(#{<<"scenario">> := ScenarioName} = Body) ->
    case
        amoc_api_helpers_scenario_info:is_loaded(ScenarioName) of
        {true, Scenario} ->
            Users = maps:get(<<"users">>, Body, 0),
            SettingsMap = maps:get(<<"settings">>, Body, #{}),
            case read_settings(SettingsMap) of
                {ok, Settings} ->
                    amoc_dist:do(Scenario, Users, Settings);
                {error, _} = Err -> Err
            end;
        false ->
            {error, no_such_scenario}
    end;
start(_) ->
    {error, invalid_body}.

-spec stop() -> ret_value().
stop() ->
    amoc_dist:stop().

-spec add_users(body()) -> ret_value().
add_users(#{<<"users">> := Users, <<"nodes">> := Nodes}) ->
    amoc_dist:add(Users, read_nodes(Nodes));
add_users(#{<<"users">> := Users}) ->
    amoc_dist:add(Users);
add_users(_) ->
    {error, invalid_body}.

-spec remove_users(body()) -> ret_value().
remove_users(#{<<"users">> := Users, <<"nodes">> := Nodes}) ->
    amoc_dist:remove(Users, false, read_nodes(Nodes));
remove_users(#{<<"users">> := Users}) ->
    amoc_dist:remove(Users, false);
remove_users(_) ->
    {error, invalid_body}.

-spec update_settings(body()) -> ret_value().
update_settings(#{<<"settings">> := SettingsMap} = Body) ->
    case read_settings(SettingsMap) of
        {ok, Settings} ->
            case Body of
                #{<<"nodes">> := Nodes} ->
                    amoc_dist:update_settings(Settings, read_nodes(Nodes));
                _ ->
                    amoc_dist:update_settings(Settings)
            end;
        {error, _} = Err -> Err
    end;
update_settings(_) ->
    {error, invalid_body}.

read_settings(SettingsMap) ->
    try
        {ok, [read_kv(K, V) || {K, V} <- maps:to_list(SettingsMap)]}
    catch
        throw:{invalid_value, _, _, _} = Err ->
            {error, Err}
    end.

read_kv(K, V) ->
    Key = binary_to_atom(K, utf8),
    case amoc_config_env:parse_value(V) of
        {ok, Value} -> {Key, Value};
        {error, E} ->
            throw({invalid_value, K, V, E})
    end.

read_nodes(NodeList) ->
    [binary_to_atom(Node, utf8) || Node <- NodeList].