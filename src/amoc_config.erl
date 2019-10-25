%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config).

-export([get/1, get/2,
         parse_scenario_settings/1,
         get_scenario_parameter/2]).

%% exported for unittesting only.
-export([get_scenario_configuration/1]).

-export_type([scenario_configuration/0, settings/0]).

-type name() :: atom() | string().
-type value() :: any().

-type one_of() :: [any()].
-type validation_fun() :: fun((any()) -> boolean() | {true, value()}).
-type validation() :: validation_fun() | one_of() | atom().
-type reason() :: atom().

-type parameter_configuration() :: {name(), DefValue :: value(), validation()}.
-type scenario_configuration() :: [parameter_configuration()].

-type settings() :: #{name() => value()}.
-type error() :: invalid_settings() | invalid_module.
-type invalid_settings() :: {invalid_settings, [{name(), value(), reason()}]}.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(name()) -> any().
get(Name) ->
    get(Name, undefined).

-spec get(name(), any()) -> any().
get(Name, Default) when is_atom(Name) ->
    DefValue = application:get_env(amoc, Name, Default),
    get(erlang:atom_to_list(Name), DefValue);
get(Name, Default) when is_list(Name) ->
    EnvName = "AMOC_" ++ Name,
    get_from_env(EnvName, Default).

-spec parse_scenario_settings(scenario_configuration() | module()) ->
    {ok, settings()} | {error, error()}.
parse_scenario_settings(Module) when is_atom(Module) ->
    try get_scenario_configuration(Module) of
        Config -> parse_scenario_settings(Config)
    catch
        _:_ -> {error, invalid_module}
    end;
parse_scenario_settings(Config) ->
    ParametersVerification = [get_value_and_verify(C) || C <- Config],
    case [{N, V, R} || {N, V, {false, R}} <- ParametersVerification] of
        [] ->
            KeyValList = [{Name, Value} || {Name, Value, _} <- ParametersVerification],
            {ok, maps:from_list(KeyValList)};
        InvalidSettings -> {error, {invalid_settings, InvalidSettings}}
    end.

-spec get_scenario_configuration(module()) -> scenario_configuration().
get_scenario_configuration(Module) ->
    ModuleAttributes = apply(Module, module_info, [attributes]),
    RequiredVariables = proplists:get_all_values(required_variable, ModuleAttributes),
    [process_var_attr(Module, Var) || Var <- lists:append(RequiredVariables)].


-spec get_scenario_parameter(name(), settings()) -> {ok, value()} | {error, no_parameter}.
get_scenario_parameter(Name, Settings) ->
    case maps:find(Name, Settings) of
        {ok, Value} -> {ok, Value};
        error -> {error, no_parameter}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_value_and_verify({Name, DefaultValue, VerificationMethod}) ->
    Value = amoc_config:get(Name, DefaultValue),
    {DefaultValueVerification, _} = verify(DefaultValue, VerificationMethod),
    {ValueVerification, NewValue} = verify(Value, VerificationMethod),
    Verification = case {DefaultValueVerification, ValueVerification} of
                       {true, true} -> true;
                       {false, true} ->
                           lager:error("Invalid default value for ~p", [Name]),
                           {false, bad_default_value};
                       {true, false} ->
                           lager:error("Invalid value for ~p", [Name]),
                           {false, bad_value};
                       {false, false} ->
                           lager:error("Invalid value & default value for ~p", [Name]),
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
    lager:error("invalid verification method ~p", [VerificationMethod]),
    false.

call_verify_fun(Fun, Value) ->
    try Fun(Value) of
        Bool when is_boolean(Bool) -> Bool;
        {true, NewValue} -> {true, NewValue};
        Ret ->
            lager:error("invalid verification method ~p, return value : ~p", [Fun, Ret]),
            false
    catch
        C:E:S ->
            lager:error("invalid verification method ~p, exception: ~p ~p ~p", [Fun, C, E, S]),
            false
    end.

is_one_of(Element, List) -> lists:any(fun(El) -> El =:= Element end, List).

get_from_env(EnvName, Default) ->
    Value = case os:getenv(EnvName) of
                false ->
                    os:getenv(string:uppercase(EnvName));
                String ->
                    String
            end,
    parse_value(Value, Default).

-spec parse_value(string() | false, any()) -> any().
parse_value(false, Default) -> Default;
parse_value("", Default) -> Default;
parse_value(String, _) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

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
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, Atom, 1) of
        true -> fun Module:Atom/1;
        false -> Atom
    end.
