%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config_scenario).

%% API
-export([parse_scenario_settings/2,
         update_settings/1,
         get_default_configuration/1,
         get_current_configuration/0]).

-include("amoc_config.hrl").

-type module_configuration_map() :: #{name() => #{value := any(),
                                                  any() => any()}}.
%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec parse_scenario_settings(module(), settings()) -> ok | error().
parse_scenario_settings(Module, Settings) when is_atom(Module) ->
    PipelineActions = [
        {fun get_configuration/1, [Module]},
        {fun verify_settings/2, [Settings]},
        {fun amoc_config_verification:process_scenario_config/2, [Settings]},
        {fun amoc_config_utils:store_scenario_config/1, []}],
    case amoc_config_utils:pipeline(PipelineActions, ok) of
        ok -> ok;
        {error, _, _} = Error -> Error;
        UnexpectedReturnValue ->
            {error, invalid_return_value, UnexpectedReturnValue}
    end.

-spec update_settings(settings()) -> ok | error().
update_settings(Settings) ->
    PipelineActions = [
        {fun get_existing_configuration/0, []},
        {fun verify_settings/2, [Settings]},
        {fun filter_configuration/2, [Settings]},
        {fun amoc_config_verification:process_scenario_config/2, [Settings]},
        {fun store_scenario_config/1, []}],
    case amoc_config_utils:pipeline(PipelineActions, ok) of
        ok -> ok;
        {error, _, _} = Error -> Error;
        UnexpectedReturnValue ->
            {error, invalid_return_value, UnexpectedReturnValue}
    end.

-spec get_default_configuration(module()) -> {ok, module_configuration_map()} | error().
get_default_configuration(Module) ->
    PipelineActions = [
        {fun get_configuration/1, []},
        {fun convert_to_config_map/1, []}],
    amoc_config_utils:pipeline(PipelineActions, {ok, Module}).

-spec get_current_configuration() -> {ok, module_configuration_map()}.
get_current_configuration() ->
    PipelineActions = [
        {fun get_existing_configuration/0, []},
        {fun convert_to_config_map/1, []}],
    amoc_config_utils:pipeline(PipelineActions, ok).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_configuration(module()) ->
    {ok, module_configuration()} | error().
get_configuration(Module) ->
    ConfigurableModules = amoc_scenario:list_configurable_modules(),
    AllConfigurableModules = [Module | ConfigurableModules],
    PipelineActions = [
        {fun compose_configuration/1, [AllConfigurableModules]},
        {fun override_configuration/2, [Module]}],
    amoc_config_utils:pipeline(PipelineActions, ok).

-spec compose_configuration([module()]) ->
    {ok, module_configuration()} | error().
compose_configuration(AllConfigurableModules) ->
    compose_configuration([], AllConfigurableModules).

-spec override_configuration(module_configuration(), module()) ->
    {ok, module_configuration()} | error().
override_configuration(OldConfig, Module) ->
    AttrName = override_variable,
    case amoc_config_attributes:get_module_configuration(AttrName, Module) of
        {ok, NewConfig} ->
            amoc_config_utils:override_config(OldConfig, NewConfig);
        {error, _, _} = Error -> Error
    end.

-spec compose_configuration(module_configuration(), [module()]) ->
    {ok, module_configuration()} | error().
compose_configuration(Config, []) ->
    {ok, Config};
compose_configuration(Config, [M | L]) ->
    AttrName = required_variable,
    case amoc_config_attributes:get_module_configuration(AttrName, M) of
        {ok, NewConfig} ->
            case amoc_config_utils:merge_config(Config, NewConfig) of
                {ok, MergedConfig} ->
                    compose_configuration(MergedConfig, L);
                {error, _, _} = Error -> Error
            end;
        {error, _, _} = Error -> Error
    end.

verify_settings(Config, Settings) ->
    verify_settings([], Config, Settings).

verify_settings([], Config, []) ->
    {ok, Config};
verify_settings(UndefinedParameters, _Config, []) ->
    {error, undefined_parameters, UndefinedParameters};
verify_settings(UndefinedParameters, Config, [{Name, _} | T]) ->
    case lists:keyfind(Name, #module_parameter.name, Config) of
        false ->
            verify_settings([Name | UndefinedParameters], Config, T);
        Tuple when is_tuple(Tuple) ->
            verify_settings(UndefinedParameters, Config, T)
    end.

get_existing_configuration() ->
    {ok, ets:tab2list(amoc_config)}.

filter_configuration(Config, Settings) ->
    Keys = proplists:get_keys(Settings),
    KeyPos = #module_parameter.name,
    FilteredConfig = [lists:keyfind(Name, KeyPos, Config) || Name <- Keys],
    case [{N, M} || #module_parameter{name = N, mod = M,
                                      update_fn = read_only} <- FilteredConfig] of
        [] -> {ok, FilteredConfig};
        ReadOnlyParameters ->
            {error, readonly_parameters, ReadOnlyParameters}
    end.

store_scenario_config(Config) ->
    amoc_config_utils:store_scenario_config(Config),
    [spawn(fun() -> apply(Fn, [Name, Value]) end)
     || #module_parameter{name = Name, value = Value, update_fn = Fn} <- Config],
    ok.

convert_to_config_map(Config) ->
    PropList = [{Name, parameter_to_map(P)}
                || #module_parameter{name = Name} = P <- Config],
    {ok, maps:from_list(PropList)}.

parameter_to_map(#module_parameter{} = Param) ->
    RecordFields = record_info(fields, module_parameter),
    RecordSize = record_info(size, module_parameter),
    FieldsWithPosition = lists:zip(lists:seq(2, RecordSize), RecordFields),
    PropList = [{Field, element(Pos, Param)} || {Pos, Field} <- FieldsWithPosition,
                                                filter_parameter_fields(Field)],
    maps:from_list(PropList).

filter_parameter_fields(name) -> false;
filter_parameter_fields(_)    -> true.
