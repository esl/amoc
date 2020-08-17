%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_scenario_status).
%% API
-export([is_loaded/1,
         scenario_settings/1,
         scenario_params/1,
         get_edoc/1,
         format/1]).

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

scenario_settings(Scenario) ->
    {ok, ConfigMap} = amoc_config_scenario:get_default_configuration(Scenario),
    F = fun(Name, #{value := Value}, NewMap) ->
            NewMap#{format(Name) => format(Value)}
        end,
    maps:fold(F, #{}, ConfigMap).

scenario_params(Scenario) ->
    {ok, ConfigMap} = amoc_config_scenario:get_default_configuration(Scenario),
    F = fun(Name, Param, NewMap) ->
            NewMap#{format(Name) => format_param(Param)}
        end,
    maps:fold(F, #{}, ConfigMap).

format_param(ParamMap) ->
    F = fun(Item, Value, NewMap) ->
            case add_item(Item) of
                {true, FormattedItem} -> NewMap#{FormattedItem => format(Value)};
                false -> NewMap
            end
        end,
    maps:fold(F, #{}, ParamMap).

add_item(mod)   -> {true, format(module)};
add_item(value) -> {true, format(default_value)};
add_item(I) ->
    RequiredItems = [description, verification_fn, update_fn],
    case lists:member(I, RequiredItems) of
        false -> false;
        true -> {true, format(I)}
    end.

-spec format(any()) -> binary().
format(Value) ->
    list_to_binary(lists:flatten(io_lib:format("~tp", [Value]))).

-spec is_loaded(binary()) -> {true, amoc:scenario()} | false.
is_loaded(ScenarioName) ->
    case to_existing_atom(ScenarioName) of
        {true, Scenario} ->
            case amoc_scenario:does_scenario_exist(Scenario) of
                true -> {true, Scenario};
                false -> false
            end;
        false ->
            false
    end.

-spec to_existing_atom(binary()) -> {true, atom()} | false.
to_existing_atom(ScenarioName) ->
    try
        {true, binary_to_existing_atom(ScenarioName, utf8)}
    catch
        error:badarg -> false
    end.
