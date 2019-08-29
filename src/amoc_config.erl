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

-type name() :: atom().
-type env_var() :: atom().
-type value() :: any().

-type one_of() :: [any()].
-type func() :: fun((any()) -> boolean()).
-type validation() :: func() | one_of() | bitstring | nonnegative_integer | positive_integer | none.
-type reason() :: atom().

-type parameter_configuration() :: {name(), env_var(), DefValue :: value(), validation()}.
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
get(Name, Default) ->
    case os:getenv("AMOC_" ++ erlang:atom_to_list(Name)) of
        false ->
            application:get_env(amoc, Name, Default);
        Value ->
            parse_value(Value)
    end.

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
get_value_and_verify({Name, Env, DefaultValue, VerificationMethod}) ->
    Value = amoc_config:get(Env, DefaultValue),
    DefaultValueVerification = verify_value(DefaultValue, VerificationMethod),
    ValueVerification = verify_value(Value, VerificationMethod),
    Verification = case {DefaultValueVerification, ValueVerification} of
                       {true, true} -> true;
                       {false, true} ->
                           lager:error("Invalid default value for ~p", [Name]),
                           {false, bad_default_value};
                       {true, false} ->
                           lager:error("Invalid value for ~p", [Name]),
                           {false, bad_value};
                       {false, false} ->
                           lager:error("Invalid default value & value for ~p", [Name]),
                           {false, bad_value_bad_default_value}
                   end,
    {Name, Value, Verification}.

verify_value(_, none) -> true;
verify_value(Value, positive_integer) ->
    is_positive_integer(Value);
verify_value(Value, nonnegative_integer) ->
    is_nonnegative_integer(Value);
verify_value(Value, bitstring) ->
    is_bitstring(Value);
verify_value(Value, [_ | _] = NonemptyList) ->
    is_one_of(Value, NonemptyList);
verify_value(Value, Fun) when is_function(Fun, 1) ->
    Fun(Value);
verify_value(_, VerificationMethod) ->
    lager:error("invalid verification method ~p", [VerificationMethod]),
    false.

is_positive_integer(I) -> is_integer(I) andalso I > 0.

is_nonnegative_integer(I) -> is_integer(I) andalso I >= 0.

is_one_of(Element, List) -> lists:any(fun(El) -> El =:= Element end, List).

-spec parse_value(string()) -> any().
parse_value(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
