%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config_scenario).

%% API
-export([parse_scenario_settings/2]).

-ifdef(TEST).
-export([%% exported for testing only
         get_module_attributes/1,
         process_scenario_attributes/2,
         process_scenario_config/2]).
-endif.

-export_type([settings/0]).

-type reason() :: any().
-type validation_fun() :: fun((amoc_config:value()) -> boolean() |
                                                       {true, amoc_config:value()} |
                                                       {false, reason()}).
-type parameter_configuration() :: {amoc_config:name(),
                                    DefValue :: amoc_config:value(),
                                    validation_fun()}.
-type scenario_configuration() :: [parameter_configuration()].

-type parameter() :: {amoc_config:name(), amoc_config:value()}.
-type settings() :: [parameter()].

-type error_type() :: invalid_scenario_module | invalid_verification_module |
                      invalid_attribute_format | parameters_verification_failed.
-type error() :: {error, error_type(), reason()}.

-type one_of() :: [amoc_config:value(), ...].
-type verification_method() :: one_of() | atom() | validation_fun().
-type scenario_attribute() ::
    {amoc_config:name(), Description :: string()} |
    {amoc_config:name(), Description :: string(), DefValue :: amoc_config:value()} |
    {amoc_config:name(), Description :: string(), DefValue :: amoc_config:value(), verification_method()}.
-type maybe_scenario_attribute() :: scenario_attribute() | term().

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec parse_scenario_settings(module(), settings()) -> ok | error().
parse_scenario_settings(Module, Settings) when is_atom(Module) ->
    try get_scenario_configuration(Module) of
        {ok, Config} ->
            case process_scenario_config(Config, Settings) of
                {ok, ScenarioSettings} ->
                    store_scenario_settings(ScenarioSettings),
                    ok;
                Error -> Error
            end;
        {error, _, _} = Error -> Error
    catch
        _:_ -> {error, invalid_scenario_module, Module}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parsing scenario attributes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_scenario_configuration(module()) -> {ok,scenario_configuration()} | error().
get_scenario_configuration(Module) ->
    case load_verification_modules() of
        {ok, VerificationModules} ->
            ScenarioAttributes = get_module_attributes(Module),
            AllVerificationModules = [Module | VerificationModules],
            process_scenario_attributes(AllVerificationModules, ScenarioAttributes);
        Error -> Error
    end.

-spec load_verification_modules() -> {ok, [module()]} | error().
load_verification_modules() ->
    Modules = amoc_config_env:find_all_vars(config_verification_modules),
    LoadingResult = [load_module(Module) || Module <- Modules],
    maybe_error(invalid_verification_module, LoadingResult).

-spec load_module(module()) -> {ok, module()} | {error, {module(), any()}}.
load_module(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> {ok, Module};
        {error, Error} -> {error, {Module, Error}}
    end.

-spec get_module_attributes(module()) -> [maybe_scenario_attribute()].
get_module_attributes(Module) ->
    ModuleAttributes = apply(Module, module_info, [attributes]),
    RequiredVariables = proplists:get_all_values(required_variable, ModuleAttributes),
    lists:append(RequiredVariables).

-spec process_scenario_attributes([module()], [maybe_scenario_attribute()]) ->
    {ok, scenario_configuration()} | error().
process_scenario_attributes(VerificationModules, ScenarioAttributes) ->
    Config = [process_var_attr(VerificationModules, Attr) || Attr <- ScenarioAttributes],
    maybe_error(invalid_attribute_format, Config).

-spec process_var_attr([module()], maybe_scenario_attribute()) ->
    {ok, parameter_configuration()} | {error, reason()}.
process_var_attr(_, {Name, _}) when is_atom(Name) ->
    {ok, {Name, undefined, fun none/1}};
process_var_attr(_, {Name, _, DefaultValue}) when is_atom(Name) ->
    {ok, {Name, DefaultValue, fun none/1}};
process_var_attr(Modules, {Name, _, DefaultValue, Atom} = Attribute) when is_atom(Atom),
                                                                          is_atom(Name) ->
    case verification_method(Modules, Atom) of
        not_exported -> {error, {verification_method_not_exported, Attribute, Modules}};
        VerificationMethod -> {ok, {Name, DefaultValue, VerificationMethod}}
    end;
process_var_attr(_, {Name, _, DefaultValue, Fun}) when is_function(Fun, 1),
                                                       is_atom(Name) ->
    {ok, {Name, DefaultValue, Fun}};
process_var_attr(_, {Name, _, DefaultValue, [_ | _] = OneOF}) when is_atom(Name) ->
    {ok, {Name, DefaultValue, one_of_fun(OneOF)}};
process_var_attr(_, {Name, _, _, _} = InvalidAttribute) when is_atom(Name) ->
    {error, {invalid_verification_method, InvalidAttribute}};
process_var_attr(_, InvalidAttribute) ->
    {error, {invalid_attribute, InvalidAttribute}}.

-spec verification_method([module()], atom()) -> validation_fun() | not_exported.
verification_method(_, none) -> fun none/1;
verification_method([], _) ->
    not_exported;
verification_method([Module | T], Atom) ->
    case erlang:function_exported(Module, Atom, 1) of
        true -> fun Module:Atom/1;
        false -> verification_method(T, Atom)
    end.

none(_) -> true.

-spec one_of_fun(one_of()) -> validation_fun().
one_of_fun(OneOf) ->
    fun(X) ->
        case lists:member(X, OneOf) of
            true -> true;
            false -> {false, {not_one_of, OneOf}}
        end
    end.

-spec maybe_error(error_type(), [{error, reason()} | {ok, any()}]) -> error() | {ok, [any()]}.
maybe_error(ErrorType, List) ->
    case [Error || {error, Error} <- List] of
        [] ->
            {ok, [CorrectValue || {ok, CorrectValue} <- List]};
        Error ->
            {error, ErrorType, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% processing scenario configuration %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec process_scenario_config(scenario_configuration(), settings()) ->
    {ok, settings()} | error().
process_scenario_config(Config, Settings) ->
    ParametersVerification = [get_value_and_verify(C, Settings) || C <- Config],
    maybe_error(parameters_verification_failed, ParametersVerification).

-spec get_value_and_verify(parameter_configuration(), settings()) ->
    {ok, parameter()} | {error, reason()}.
get_value_and_verify({Name, Default, VerificationMethod}, Settings) ->
    DefaultValue = amoc_config_env:get(amoc, Name, Default),
    Value = proplists:get_value(Name, Settings, DefaultValue),
    case verify(VerificationMethod, Value) of
        {true, NewValue} -> {ok, {Name, NewValue}};
        {false, Reason} -> {error, {Name, Value, Reason}}
    end.


-spec verify(validation_fun(), amoc_config:value()) -> {true, amoc_config:value()} |
                                                       {false, reason()}.
verify(Fun, Value) ->
    try apply(Fun, [Value]) of
        true -> {true, Value};
        false -> {false, verification_failed};
        {true, NewValue} -> {true, NewValue};
        {false, Reason} -> {false, {verification_failed, Reason}};
        Ret ->
            ?LOG_ERROR("invalid verification method ~p(~p), return value : ~p",
                       [Fun, Value, Ret]),
            {false, {invalid_verification_return_value, Ret}}
    catch
        C:E:S ->
            ?LOG_ERROR("invalid verification method ~p(~p), exception: ~p ~p ~p",
                       [Fun, Value, C, E, S]),
            {false, {exception_durin_verification, {C, E, S}}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% store scenario settings %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec store_scenario_settings(settings()) -> true.
store_scenario_settings(Settings) ->
    ValidSettings = [{K, V} || {K, V} <- Settings, is_atom(K)],
    true = ets:insert(amoc_config, ValidSettings).