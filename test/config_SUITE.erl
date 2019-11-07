-module(config_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable({var1, "var1"}).
-required_variable({var2, "var2", def2}).
-required_variable([
    {var3, "var3", def3, some_atom},
    {var4, "var4", def4, [def4, another_atom]},
    {var5, "var5", def5, test_verification_function},
    {var6, "var6", def6, fun ?MODULE:test_verification_function/1}
]).

-export([test_verification_function/1]).
test_verification_function(_) -> true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [config_osenv_test,
     config_appenv_test,
     config_none_test,
     config_osappenv_test,
     config_osenv_dynamic_test,
     get_scenario_configuration,
     parse_scenario_settings_uses_default_values,
     parse_scenario_settings_shadows_default_values,
     parse_scenario_settings_returns_error_for_invalid_values,
     parse_scenario_settings_returns_preprocessed_value].

config_osenv_test(_Config) ->
    given_osenv_set([{interarrival, 100},
                     {something, [{a,b,c}, 9, 8, {7}]}]),
    given_amoc_started(),
    ?assertEqual(100, amoc_config:get(interarrival)),
    ?assertEqual(60000, amoc_config:get(repeat_interval)),
    ?assertEqual(something, amoc_config:get(anything, something)),
    ?assertEqual([{a,b,c},9,8,{7}], amoc_config:get(something)).

given_amoc_started() ->
    {ok, _} = application:ensure_all_started(amoc).

given_osenv_set(Envs) ->
    [true = set_env(Name, Value) || {Name, Value} <- Envs].

config_appenv_test(_) ->
    %% given
    given_amoc_started(),
    %% when
    application:set_env(amoc, foo, bar),
    %% then
    ?assertEqual(bar, amoc_config:get(foo)).

config_none_test(_) ->
    %% when
    application:unset_env(amoc, foo),
    %% then
    ?assertEqual(undefined, amoc_config:get(foo)).

config_osappenv_test(_) ->
    %% given
    given_amoc_started(),
    %% when
    set_env(foo, bar),
    application:set_env(amoc, foo, baz),
    %% then
    ?assertEqual(bar, amoc_config:get(foo)).

config_osenv_dynamic_test(_) ->
    %% given
    given_amoc_started(),
    %% when
    set_env(foo, bar),
    %% then
    ?assertEqual(bar, amoc_config:get(foo)),
    %% when
    set_env(foo, baz),
    %% then
    ?assertEqual(baz, amoc_config:get(foo)).

get_scenario_configuration(_) ->
    Result = amoc_config:get_scenario_configuration(?MODULE),
    ExpectedResult = [{var1, undefined, none},
                      {var2, def2, none},
                      {var3, def3, some_atom},
                      {var4, def4, [def4, another_atom]},
                      {var5, def5, fun ?MODULE:test_verification_function/1},
                      {var6, def6, fun ?MODULE:test_verification_function/1}],
    ?assertEqual(lists:sort(ExpectedResult), lists:sort(Result)).

parse_scenario_settings_uses_default_values(_) ->
    given_amoc_started(),
    ScenarioParams = correct_scenario_params(),
    given_scenario_parameters_not_set(ScenarioParams),
    Result = amoc_config:parse_scenario_settings(ScenarioParams),
    assert_settings(Result, ScenarioParams).

parse_scenario_settings_shadows_default_values(_) ->
    given_amoc_started(),
    ScenarioParams = correct_scenario_params(),
    given_scenario_parameters_set(ScenarioParams),
    AnotherScenarioParams = another_correct_scenario_params(),
    Result = amoc_config:parse_scenario_settings(AnotherScenarioParams),
    assert_settings(Result, ScenarioParams).

parse_scenario_settings_returns_error_for_invalid_values(_) ->
    given_amoc_started(),
    IncorrectScenarioParams = incorrect_scenario_params(),
    Defaults = correct_scenario_params(),

    given_scenario_parameters_set([{a_name, 3, none} | IncorrectScenarioParams]),
    Result = amoc_config:parse_scenario_settings([{a_name, <<"bitstring">>, positive_integer},
                                                  {another_name, an_atom, bitstring} | Defaults]),
    ?assertMatch({error,
                  {invalid_settings,
                   [{a_name, 3, bad_default_value},
                    {another_name, an_atom, bad_value_bad_default_value},
                    {pos_int, 0, bad_value},
                    {nonneg_int, -1, bad_value},
                    {atom, wrong_atom, bad_value},
                    {some_bitstring, an_atom, bad_value},
                    {another_bitstring, "string", bad_value}]}}, Result).

parse_scenario_settings_returns_preprocessed_value(_) ->
    given_amoc_started(),
    ValidationFn = fun(_) -> {true, another_atom} end,
    ScenarioParams = [{preprocessed_param, an_atom, ValidationFn}],
    {ok, Settings} = amoc_config:parse_scenario_settings(ScenarioParams),
    ?assertEqual({ok, another_atom}, amoc_config:get_scenario_parameter(preprocessed_param, Settings)).

assert_settings(Result, ScenarioParams) ->
    ?assertMatch({ok, SettingsMap} when is_map(SettingsMap), Result),
    {ok, Settings} = Result,
    ?assertEqual(map_size(Settings), length(ScenarioParams)),
    [?assertEqual({ok, Value}, amoc_config:get_scenario_parameter(Name, Settings)) ||
        {Name, Value, _} <- ScenarioParams].

correct_scenario_params() ->
    scenario_params(10000, 0, some_atom, <<"some_bitstring">>, <<"another_bitstring">>).

another_correct_scenario_params() ->
    scenario_params(20000, 3, another_atom, <<"yet_another_bitstring">>, <<"bitstring">>).

incorrect_scenario_params() ->
    scenario_params(0, -1, wrong_atom, an_atom, "string").

scenario_params(PosInt, NonNegInt, Atom, BitString1, BitString2) ->
    [
        {pos_int, PosInt, positive_integer},
        {nonneg_int, NonNegInt, nonnegative_integer},
        {atom, Atom, [some_atom, another_atom]},
        {some_bitstring, BitString1, bitstring},
        {another_bitstring, BitString2, fun erlang:is_bitstring/1}
    ].

given_scenario_parameters_not_set(ScenarioParams) ->
    [unset_env(Name) || {Name, _, _} <- ScenarioParams].

given_scenario_parameters_set(ScenarioParams) ->
    [set_env(Name, Value) || {Name, Value, _} <- ScenarioParams].

unset_env(Name) ->
    EnvName = env_name(Name),
    os:unsetenv(EnvName),
    false = os:getenv(EnvName).

set_env(Name, Value) ->
    os:putenv(env_name(Name), format_value(Value)).

env_name(Name) ->
    "AMOC_" ++ string:uppercase(erlang:atom_to_list(Name)).

format_value(Value) ->
    io_lib:format("~tp", [Value]).



