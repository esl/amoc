%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config).

-export([get/1,
         get/2]).

-export([parse_scenario_settings/1,
         get_scenario_parameter/2]).

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
-type invalid_settings() :: {invalid_settings, [{name(), value(), reason()}]}.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(name()) -> any().
get(Name) ->
    get(Name, undefined).

-spec get(name(), any()) -> any().
get(Name, Default) when is_atom(Name) ->
    EnvName = "AMOC_" ++ erlang:atom_to_list(Name),
    case get_from_env(EnvName) of
        false ->
            application:get_env(amoc, Name, Default);
        Value ->
            Value
    end;
get(Name, Default) when is_list(Name) ->
    EnvName = "AMOC_" ++ Name,
    case get_from_env(EnvName) of
        false ->
            Default;
        Value ->
            Value
    end.
%%
%% ------------------------------------------------------------------
%% @doc
%% This function reads scenarion parameters from env variables only
%% without consulting application envs.
%% The env name is a uppercase representation of the atom name passed in the list
%% of parameters.
%% For example: name iq_timeout is translated to AMOC_IQ_TIMEOUT env var name
%% ------------------------------------------------------------------
-spec parse_scenario_settings(scenario_configuration()) -> {ok, settings()} | {error, invalid_settings()}.
parse_scenario_settings(Config) ->
    ParametersVerification = [get_value_and_verify(C) || C <- Config],
    case [{N, V, R} || {N, V, {false, R}} <- ParametersVerification] of
        [] ->
            KeyValList = [{Name, Value} || {Name, Value, _} <- ParametersVerification],
            {ok, maps:from_list(KeyValList)};
        InvalidSettings -> {error, {invalid_settings, InvalidSettings}}
    end.

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
    EnvName = string:uppercase(erlang:atom_to_list(Name)),
    Value = amoc_config:get(EnvName, DefaultValue),
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

get_from_env(EnvName) ->
    case os:getenv(EnvName) of
        false ->
            false;
        Value ->
            parse_value(Value)
    end.

-spec parse_value(string()) -> any().
parse_value(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
