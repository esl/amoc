%%%-------------------------------------------------------------------
%%% @author denys
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Mar 2019 01:15
%%%-------------------------------------------------------------------
-module(amoc_scenario_config).
-author("denys").

%% API
-export([get_scenario_settings/1, store_scenario_settings/1, get_parameter/1, dump_settings/0]).

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

-type settings() :: [{name(), value()}].
-type invalid_settings() :: {invalid_settings, [{name(), value(), reason()}]}.

-spec get_scenario_settings(scenario_configuration()) -> {ok, settings()} | {error, invalid_settings()}.
get_scenario_settings(Config) ->
    ParametersVerification = [get_value_and_verify(C) || C <- Config],
    case [{N, V, R} || {N, V, {false, R}} <- ParametersVerification] of
        [] -> {ok, [{Name, Value} || {Name, Value, _} <- ParametersVerification]};
        InvalidSettings -> {error, {invalid_settings, InvalidSettings}}
    end.

-spec store_scenario_settings(settings()) -> any().
store_scenario_settings(Settings) ->
    erlang:put(scenario_settings, Settings).

-spec get_parameter(name()) -> {ok, value()} | {error, no_settings | no_parameter}.
get_parameter(Name) ->
    case erlang:get(scenario_settings) of
        undefined -> {error, no_settings};
        Settings ->
            case lists:keyfind(Name, 1, Settings) of
                false -> {error, no_parameter};
                {Name, Value} -> {ok, Value}
            end
    end.

-spec dump_settings() -> any().
dump_settings() ->
    case erlang:get(scenario_settings) of
        undefined -> lager:error("no scenario settings");
        Settings -> lager:info("scenario settings: ~p", [Settings])
    end.

get_value_and_verify({Name, Env, DefaultValue, VerificationMethod}) ->
    Value = amoc_config:get(Env, DefaultValue),
    DefaultValueVerification = verify_value(DefaultValue, VerificationMethod),
    ValueVerification = verify_value(Value, VerificationMethod),
    Verification = case {DefaultValueVerification, ValueVerification} of
                       {true, true} -> true;
                       {true, false} ->
                           lager:error("Invalid default value for ~p", [Name]),
                           {false, bad_default_value};
                       {false, true} ->
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

