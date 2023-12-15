%% @see amoc_config
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc This module is responsible for processing scenario configuration.
%% It applies the verification function provided in the `required_variable' parameter to the respective value
%% @end
-module(amoc_config_verification).

%% API
-export([process_scenario_config/2]).

-include("amoc_config.hrl").

%% @doc Applies the processing as provided by the `required_variable' list to the provided scenario config
-spec process_scenario_config(module_configuration(), settings()) ->
    {ok, module_configuration()} | error().
process_scenario_config(Config, Settings) ->
    ParametersVerification = [get_value_and_verify(Param, Settings) || Param <- Config],
    amoc_config_utils:maybe_error(parameters_verification_failed, ParametersVerification).

-spec get_value_and_verify(module_parameter(), settings()) ->
    {ok, module_parameter()} | {error, reason()}.
get_value_and_verify(#module_parameter{name = Name, value = Default,
                                       verification_fn = VerificationFn} = Param,
                     Settings) ->
    DefaultValue = amoc_config_env:get(Name, Default),
    Value = proplists:get_value(Name, Settings, DefaultValue),
    case verify(VerificationFn, Value) of
        {true, NewValue} ->
            {ok, Param#module_parameter{value = NewValue}};
        {false, Reason} ->
            {error, {Name, Value, Reason}}
    end.

-spec verify(maybe_verification_fun(), value()) -> {true, value()} | {false, reason()}.
verify(Fun, Value) ->
    try apply(Fun, [Value]) of
        true -> {true, Value};
        false -> {false, verification_failed};
        {true, NewValue} -> {true, NewValue};
        {false, Reason} -> {false, {verification_failed, Reason}};
        Ret ->
            amoc_telemetry:execute_log(
              error, [config, verify],
              #{verification_method => Fun, verification_arg => Value, verification_return => Ret},
              <<"invalid verification method">>),
            {false, {invalid_verification_return_value, Ret}}
    catch
        C:E:S ->
            amoc_telemetry:execute_log(
              error, [config, verify],
              #{verification_method => Fun, verification_arg => Value,
                kind => C, reason => E, stacktrace => S},
              <<"invalid verification method">>),
            {false, {exception_during_verification, {C, E, S}}}
    end.
