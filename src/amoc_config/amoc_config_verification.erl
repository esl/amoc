%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% this module is responsible for processing scenario configuration
%%==============================================================================
-module(amoc_config_verification).

%% API
-export([process_scenario_config/2]).

-include("amoc_config.hrl").

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
            telemetry:execute([amoc, config, verify], #{error => 1},
                              #{log_class => error, verification_method => Fun,
                                verification_arg => Value, verification_return => Ret,
                                msg => <<"invalid verification method">>}),
            {false, {invalid_verification_return_value, Ret}}
    catch
        C:E:S ->
            telemetry:execute([amoc, config, verify], #{error => 1},
                              #{log_class => error, verification_method => Fun,
                                verification_arg => Value,
                                kind => C, reason => E, stacktrace => S,
                                msg => <<"invalid verification method">>}),
            {false, {exception_during_verification, {C, E, S}}}
    end.
