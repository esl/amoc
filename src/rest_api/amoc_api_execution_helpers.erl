%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_execution_helpers).
-author("denysgonchar").

%% API
-export([start/1, stop/0, add_users/1, remove_users/1, update_settings/1]).

start(#{<<"scenario">> := ScenarioName} = Body) ->
    case
        amoc_api_scenario_status:is_loaded(ScenarioName) of
        {true, Scenario} ->
            Users = maps:get(<<"users">>, Body, 0),
            SettingsMap = maps:get(<<"settings">>, Body, #{}),
            Settings = read_settings(SettingsMap),
            case amoc_dist:do(Scenario, Users, Settings) of
                {ok, _} -> {200, #{}, #{}};
                {error, _} -> {409, #{}, #{}}
            end;
        false ->
            {409, #{}, #{}}
    end.

stop() ->
    case amoc_dist:stop() of
        {ok, _} ->
            {200, #{}, #{}};
        {error, _} ->
            {409, #{}, #{}}
    end.

add_users(#{<<"users">> := Users} = Body) ->
    Result = case Body of
                 #{<<"nodes">> := Nodes} -> amoc_dist:add(Users, read_nodes(Nodes));
                 _ -> amoc_dist:add(Users)
             end,
    case Result of
        {ok, _} ->
            {200, #{}, #{}};
        {error, _} ->
            {409, #{}, #{}}
    end.

remove_users(#{<<"users">> := Users} = Body) ->
    Result = case Body of
                 #{<<"nodes">> := Nodes} -> amoc_dist:remove(Users, false, read_nodes(Nodes));
                 _ -> amoc_dist:remove(Users, false)
             end,
    case Result of
        {ok, _} ->
            {200, #{}, #{}};
        {error, _} ->
            {409, #{}, #{}}
    end.

update_settings(#{<<"settings">> := SettingsMap} = Body) ->
    Settings = read_settings(SettingsMap),
    Result = case Body of
                 #{<<"nodes">> := Nodes} -> amoc_dist:update_settings(Settings, read_nodes(Nodes));
                 _ -> amoc_dist:update_settings(Settings)
             end,
    case Result of
        {ok, _} ->
            {200, #{}, #{}};
        {error, _} ->
            {409, #{}, #{}}
    end.

read_settings(SettingsMap) ->
    [read_kv(K, V) || {K, V} <- maps:to_list(SettingsMap)].

read_kv(K, V) ->
    Key = binary_to_atom(K, utf8),
    {ok, Value} = amoc_config_env:parse_value(V),
    {Key, Value}.

read_nodes(NodeList) ->
    [binary_to_atom(Node, utf8) || Node <- NodeList].