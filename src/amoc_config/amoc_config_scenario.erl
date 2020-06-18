%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config_scenario).

%% API
-export([parse_scenario_settings/2]).

-include_lib("kernel/include/logger.hrl").
-include("amoc_config.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec parse_scenario_settings(module(), settings()) -> ok | error().
parse_scenario_settings(Module, Settings) when is_atom(Module) ->
    PipelineActions = [
        {fun amoc_config_utils:load_verification_modules/0, []},
        {fun get_configuration/2, [Module]},
        {fun amoc_config_validation:process_scenario_config/2, [Settings]},
        {fun amoc_config_utils:store_scenario_config/1, []}],
    case amoc_config_utils:pipeline(PipelineActions, ok) of
        ok -> ok;
        {error, _, _} = Error -> Error;
        UnexpectedReturnValue ->
            {error, invalid_return_value, UnexpectedReturnValue}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_configuration([module()], module()) ->
    {ok, module_configuration()} | error().
get_configuration(VerificationModules, Module) ->
    ParametrisedModules = amoc_scenario:list_parametrised_modules(),
    AllParametrisedModules = [Module | ParametrisedModules],
    PipelineActions = [
        {fun compose_configuration/2, [AllParametrisedModules, VerificationModules]},
        {fun override_configuration/3, [Module, VerificationModules]}],
    amoc_config_utils:pipeline(PipelineActions, ok).

-spec compose_configuration([module()], [module()]) ->
    {ok, module_configuration()} | error().
compose_configuration(AllParametrisedModules, VerificationModules) ->
    compose_configuration([], AllParametrisedModules, VerificationModules).

-spec override_configuration(module_configuration(), module(), [module()]) ->
    {ok, module_configuration()} | error().
override_configuration(OldConfig, Module, VerificationModules) ->
    AttrName = override_variable,
    case amoc_config_attributes:get_module_configuration(AttrName, Module,
                                                         VerificationModules) of
        {ok, NewConfig} ->
            amoc_config_utils:override_config(OldConfig, NewConfig);
        {error, _, _} = Error -> Error
    end.

-spec compose_configuration(module_configuration(), [module()], [module()]) ->
    {ok, module_configuration()} | error().
compose_configuration(Config, [], _) ->
    {ok, Config};
compose_configuration(Config, [M | L], VerificationModules) ->
    AttrName = required_variable,
    case amoc_config_attributes:get_module_configuration(AttrName, M, VerificationModules) of
        {ok, NewConfig} ->
            case amoc_config_utils:merge_config(Config, NewConfig) of
                {ok, MergedConfig} ->
                    compose_configuration(MergedConfig, L, VerificationModules);
                {error, _, _} = Error -> Error
            end;
        {error, _, _} = Error -> Error
    end.
