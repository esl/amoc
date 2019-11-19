%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config_scenario).

%% API
-export([parse_scenario_settings/2]).

%% exported for testing only
-export([get_scenario_configuration/1,process_scenario_config/2]).

-export_type([scenario_configuration/0, settings/0]).

-type one_of() :: [amoc_config:value()].
-type validation_fun() :: fun((amoc_config:value()) -> boolean() | {true, amoc_config:value()}).
-type validation() :: validation_fun() | one_of() | atom().

-type parameter_configuration() :: {amoc_config:name(),
                                    DefValue :: amoc_config:value(),
                                    validation()}.
-type scenario_configuration() :: [parameter_configuration()].

-type settings() :: [{amoc_config:name(), amoc_config:value()}].

-type reason() :: atom().
-type error() :: invalid_settings() | invalid_module.
-type invalid_settings() :: {invalid_settings, [{amoc_config:name(),
                                                 amoc_config:value(),
                                                 reason()}]}.

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec parse_scenario_settings(module(), settings()) -> ok | {error, error()}.
parse_scenario_settings(Module, Settings) when is_atom(Module) ->
    try get_scenario_configuration(Module) of
        Config ->
            case process_scenario_config(Config, Settings) of
                {ok, ScenarioSettings} ->
                    store_scenario_settings(ScenarioSettings),
                    ok;
                Error -> Error
            end
    catch
        _:_ -> {error, invalid_module}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parsing scenario attributes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_scenario_configuration(module()) -> scenario_configuration().
get_scenario_configuration(Module) ->
    ModuleAttributes = apply(Module, module_info, [attributes]),
    RequiredVariables = proplists:get_all_values(required_variable, ModuleAttributes),
    [process_var_attr(Module, Var) || Var <- lists:append(RequiredVariables)].

process_var_attr(_, {Name, _}) ->
    {Name, undefined, none};
process_var_attr(_, {Name, _, DefaultValue}) ->
    {Name, DefaultValue, none};
process_var_attr(Module, {Name, _, DefaultValue, Atom}) when is_atom(Atom) ->
    VerificationMethod = verification_method(Module, Atom),
    {Name, DefaultValue, VerificationMethod};
process_var_attr(_, {Name, _, DefaultValue, VerificationMethod}) ->
    {Name, DefaultValue, VerificationMethod}.

verification_method(Module, Atom) ->
    case erlang:function_exported(Module, Atom, 1) of
        true -> fun Module:Atom/1;
        false -> Atom
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% processing scenario configuration %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec process_scenario_config(scenario_configuration(), settings()) ->
    {ok, settings()} | {error, error()}.
process_scenario_config(Config, Settings) ->
    ParametersVerification = [get_value_and_verify(C, Settings) || C <- Config],
    case [{N, V, R} || {N, V, {false, R}} <- ParametersVerification] of
        [] ->
            KeyValList = [{Name, Value} || {Name, Value, _} <- ParametersVerification],
            {ok, KeyValList};
        InvalidSettings -> {error, {invalid_settings, InvalidSettings}}
    end.

get_value_and_verify({Name, Default, VerificationMethod}, Settings) ->
    DefaultValue = amoc_config_env:get(Name, Default),
    Value = proplists:get_value(Name, Settings, DefaultValue),
    {DefaultValueVerification, _} = verify(Default, VerificationMethod),
    {ValueVerification, NewValue} = verify(Value, VerificationMethod),
    Verification = case {DefaultValueVerification, ValueVerification} of
                       {true, true} -> true;
                       {false, true} ->
                           ?LOG_ERROR("Invalid default value for ~p", [Name]),
                           {false, bad_default_value};
                       {true, false} ->
                           ?LOG_ERROR("Invalid value for ~p", [Name]),
                           {false, bad_value};
                       {false, false} ->
                           ?LOG_ERROR("Invalid value & default value for ~p", [Name]),
                           {false, bad_value_bad_default_value}
                   end,
    {Name, NewValue, Verification}.

verify(Value, VerificationMethod) ->
    case verify_value(Value, VerificationMethod) of
        {true, NewValue} -> {true, NewValue};
        true -> {true, Value};
        false -> {false, Value}
    end.

verify_value(_, none) -> true;
verify_value(Value, Atom) when is_atom(Atom) ->
    Fun = fun(Val) ->
        erlang:apply(amoc_config_validation, Atom, [Val])
          end,
    call_verify_fun(Fun, Value);
verify_value(Value, [_ | _] = NonemptyList) ->
    is_one_of(Value, NonemptyList);
verify_value(Value, Fun) when is_function(Fun, 1) ->
    call_verify_fun(Fun,Value);
verify_value(_, VerificationMethod) ->
    ?LOG_ERROR("invalid verification method ~p", [VerificationMethod]),
    false.

call_verify_fun(Fun, Value) ->
    try Fun(Value) of
        Bool when is_boolean(Bool) -> Bool;
        {true, NewValue} -> {true, NewValue};
        Ret ->
            ?LOG_ERROR("invalid verification method ~p, return value : ~p", [Fun, Ret]),
            false
    catch
        C:E:S ->
            ?LOG_ERROR("invalid verification method ~p, exception: ~p ~p ~p", [Fun, C, E, S]),
            false
    end.

is_one_of(Element, List) -> lists:any(fun(El) -> El =:= Element end, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% store scenario settings %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
store_scenario_settings(Settings) ->
    ValidSettings = [{K, V} || {K, V} <- Settings, is_atom(K)],
    true = ets:insert(amoc_config, ValidSettings).