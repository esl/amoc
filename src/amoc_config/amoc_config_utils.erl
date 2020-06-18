%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config_utils).

-include("amoc_config.hrl").

%% API
-export([load_verification_modules/0,
         maybe_error/2,
         pipeline/2,
         merge_config/2,
         override_config/2,
         store_scenario_config/1]).

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

-spec maybe_error(error_type(), [{error, reason()} | {ok, any()}]) ->
    error() | {ok, [any()]}.
maybe_error(ErrorType, List) ->
    case [Reason || {error, Reason} <- List] of
        [] ->
            {ok, [CorrectValue || {ok, CorrectValue} <- List]};
        Errors ->
            {error, ErrorType, Errors}
    end.

-spec pipeline([{function(), Args :: [any()]}], any()) -> any().
pipeline(Actions, InitValue) ->
    lists:foldl(fun run_action/2, InitValue, Actions).

run_action({Fun, Args}, {ok, FirstArg}) ->
    try_apply_fn(Fun, [FirstArg | Args]);
run_action({Fun, Args}, ok) ->
    try_apply_fn(Fun, Args);
run_action(_, Error) -> Error.

try_apply_fn(Fun, Args) ->
    try
        apply(Fun, Args)
    catch
        C:E:S -> {error, pipeline_action_crashed, {C, E, S}}
    end.

-spec merge_config(module_configuration(), module_configuration()) ->
    {ok, module_configuration()} | error().
merge_config(MergedConfig, []) ->
    {ok, MergedConfig};
merge_config(OldConfig, [{ParamName, Module, _, _} = Param | L]) ->
    case lists:keyfind(ParamName, 1, OldConfig) of
        false ->
            merge_config([Param | OldConfig], L);
        {ParamName, AnotherModule, _, _} ->
            {error, parameter_overriding, {ParamName, Module, AnotherModule}}
    end.

-spec override_config(module_configuration(), module_configuration()) ->
    {ok, module_configuration()}.
override_config(OldConfig,NewConfig)->
    ReversedNewConfig = lists:reverse(NewConfig),
    FullConfig = ReversedNewConfig ++ OldConfig,
    {ok, lists:ukeysort(1,FullConfig)}.

-spec store_scenario_config(module_configuration()) -> ok.
store_scenario_config(Config) ->
    true = ets:insert(amoc_config, Config),
    ok.