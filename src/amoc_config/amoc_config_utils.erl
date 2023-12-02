%% @private
%% @see amoc_config
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_config_utils).

-include("amoc_config.hrl").

%% API
-export([maybe_error/2,
         pipeline/2,
         merge_config/2,
         override_config/2,
         store_scenario_config/1,
         create_amoc_config_ets/0,
         find_all_vars/1]).

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
    maybe_module_config().
merge_config(MergedConfig, []) ->
    {ok, MergedConfig};
merge_config(OldConfig, [#module_parameter{name = Name, mod = Module} = Param | L]) ->
    case lists:keyfind(Name, #module_parameter.name, OldConfig) of
        false ->
            merge_config([Param | OldConfig], L);
        #module_parameter{name = Name, mod = AnotherModule} ->
            {error, parameter_overriding, {Name, Module, AnotherModule}}
    end.

-spec override_config(module_configuration(), module_configuration()) ->
    {ok, module_configuration()}.
override_config(OldConfig, NewConfig) ->
    ReversedNewConfig = lists:reverse(NewConfig),
    FullConfig = ReversedNewConfig ++ OldConfig,
    {ok, lists:ukeysort(#module_parameter.name, FullConfig)}.

-spec store_scenario_config(module_configuration()) -> ok.
store_scenario_config(Config) ->
    true = ets:insert(amoc_config, Config),
    ok.

-spec create_amoc_config_ets() -> any().
create_amoc_config_ets() ->
    amoc_config = ets:new(amoc_config, [named_table,
                                        protected,
                                        {keypos, #module_parameter.name},
                                        {read_concurrency, true}]).

-spec find_all_vars(atom()) -> [any()].
find_all_vars(Name) ->
    AllValues = [application:get_env(App, Name)
                 || {App, _, _} <- application:loaded_applications()],
    [Value || {ok, Value} <- AllValues].
