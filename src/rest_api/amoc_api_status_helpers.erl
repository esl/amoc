%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_status_helpers).
-author("denysgonchar").

%% API
-export([get_status/0]).

get_status() ->
    Apps = application:which_applications(),
    AmocStatus = case lists:keyfind(amoc, 1, Apps) of
                     {amoc, _Desc, _Vsn} -> <<"up">>;
                     false -> <<"down">>
                 end,
    Env = get_envs(),
    Status = #{amoc_status => AmocStatus, env => Env},
    maybe_add_controller_status(Status).

get_envs() ->
    [begin
         [VarName, VarValue] = string:split(V, "="),
         {list_to_binary(VarName), list_to_binary(VarValue)}
     end || "AMOC_" ++ _ = V <- os:getenv()].

maybe_add_controller_status(#{amoc_status := <<"up">>} = Status) ->
    Status#{controller => get_controller_status()};
maybe_add_controller_status(Status) -> Status.

get_controller_status() ->
    Status = case amoc_controller:get_status() of
                 idle -> #{status => <<"idle">>};
                 disabled -> #{status => <<"disabled">>};
                 {error, Error} ->
                     #{status => <<"error">>,
                       error => amoc_config_env:format(Error, binary)};
                 {terminating, Scenario} ->
                     #{status => <<"terminating">>,
                       scenario => atom_to_binary(Scenario, utf8)};
                 {finished, Scenario} ->
                     #{status => <<"finished">>,
                       scenario => atom_to_binary(Scenario, utf8)};
                 {running, Scenario, NoOfUsers, _} ->
                     #{status => <<"running">>,
                       scenario => atom_to_binary(Scenario, utf8),
                       number_of_users => NoOfUsers}
             end,
    maybe_add_settings(Status).

maybe_add_settings(#{status := S} = Status) when S =:= <<"running">>;
                                                 S =:= <<"terminating">>;
                                                 S =:= <<"finished">> ->
    Settings = get_current_settings(),
    Status#{settings => Settings};
maybe_add_settings(Status) -> Status.

get_current_settings() ->
    {ok, ConfigMap} = amoc_config_scenario:get_current_configuration(),
    F = fun(_Name, #{value := Value}) ->
            amoc_config_env:format(Value, binary)
        end,
    maps:map(F, ConfigMap).
