-module(amoc_config_scenario_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable({var1, "var1"}).
-required_variable({var2, "var2", def2}).
-required_variable([
    {var3, "var3", 3, positive_integer},
    {var4, "var4", def4, [def4, another_atom]},
    {var5, "var5", def5, test_verification_function},
    {var6, "var6", def6, fun ?MODULE:test_verification_function/1}
]).

-export([test_verification_function/1, positive_integer/1]).
test_verification_function(_) -> true.
positive_integer(I) -> is_integer(I) andalso I > 0.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [init_scenario,
     get_module_attributes,
     process_scenario_config_uses_default_values,
     process_scenario_config_shadows_default_values,
     process_scenario_config_returns_error_for_invalid_values,
     process_scenario_config_returns_preprocessed_value].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(amoc),
    meck:new(verification_module, [non_strict, no_link]),
    meck:expect(verification_module, positive_integer,
                fun(I) -> is_integer(I) andalso I > 0 end),
    meck:expect(verification_module, nonnegative_integer,
                fun(I) -> is_integer(I) andalso I >= 0 end),
    meck:expect(verification_module, binary,
                fun(Binary) -> is_binary(Binary) end),
    set_env(config_verification_modulesv, [verification_module]),
    Config.

end_per_suite(Config) ->
    meck:unload(),
    unset_env(config_verification_modulesv),
    Config.

init_scenario(_) ->
    ScenarioConfig = [{var0, def0}, {var5, val5}],
    ?assertMatch({ok, _}, amoc_controller:init_scenario(?MODULE, [{config, ScenarioConfig}])),
    ?assertEqual(undefined, amoc_config:get(var00)),
    ?assertEqual(def00, amoc_config:get(var00, def00)),
    ?assertEqual(undefined, amoc_config:get(var0)),
    ?assertEqual(def0, amoc_config:get(var0, def0)),
    ?assertEqual(undefined, amoc_config:get(var1)),
    ?assertEqual(def1, amoc_config:get(var1, def1)),
    ?assertEqual(def2, amoc_config:get(var2)),
    ?assertEqual(def2, amoc_config:get(var2, some_atom)),
    ?assertEqual(val5, amoc_config:get(var5)),
    ?assertEqual(val5, amoc_config:get(var5, some_atom)).

get_module_attributes(_) ->
    Result = amoc_config_scenario:get_module_attributes(?MODULE),
    ExpectedResult = [{var1, "var1"},
                      {var2, "var2", def2},
                      {var3, "var3", 3, positive_integer},
                      {var4, "var4", def4, [def4, another_atom]},
                      {var5, "var5", def5, test_verification_function},
                      {var6, "var6", def6, fun ?MODULE:test_verification_function/1}],
    ?assertEqual(lists:sort(ExpectedResult), lists:sort(Result)).

process_scenario_config_uses_default_values(_) ->
    ScenarioAttributes = correct_scenario_attributes(),
    given_scenario_parameters_not_set(ScenarioAttributes),
    Result = process_scenario_attributes(ScenarioAttributes,[]),
    assert_settings(Result, ScenarioAttributes).

process_scenario_config_shadows_default_values(_) ->
    ScenarioAttributes = correct_scenario_attributes(),
    given_scenario_parameters_set(ScenarioAttributes),
    AnotherScenarioAttributes = another_correct_scenario_attributes(),
    AdditionalAttribute = {additional_param, "", value, none},
    set_env(additional_param, another_value),
    Result = process_scenario_attributes(
        [AdditionalAttribute | AnotherScenarioAttributes],
        [{additional_param, yet_another_value}]),
    assert_settings(Result, [{additional_param, "", yet_another_value, none} | ScenarioAttributes]).

process_scenario_config_returns_error_for_invalid_values(_) ->
    IncorrectScenarioAttributes = incorrect_scenario_attributes(),
    Defaults = correct_scenario_attributes(),
    given_scenario_parameters_set([{a_name, 3, none} | IncorrectScenarioAttributes]),
    Result = process_scenario_attributes(
        [{a_name, <<"binary">>, positive_integer}, {another_name, an_atom, binary} | Defaults],
        [{unsued_name, whatever_value}, {nonneg_int, -5}]),
    ?assertEqual({error, parameters_verification_failed,
                  [{pos_int, 0, verification_failed},
                   {nonneg_int, -5, verification_failed},
                   {atom, wrong_atom,
                    {verification_failed,
                     {not_one_of, [some_atom, another_atom]}}},
                   {some_binary, an_atom, verification_failed},
                   {another_binary, "string", verification_failed}]},
                 Result).

process_scenario_config_returns_preprocessed_value(_) ->
    ValidationFn = fun(_) -> {true, another_atom} end,
    ScenarioParams = [{preprocessed_param, an_atom, ValidationFn}],
    {ok, Settings} = amoc_config_scenario:process_scenario_config(ScenarioParams,[]),
    ?assertEqual([{preprocessed_param, another_atom}],
                 proplists:lookup_all(preprocessed_param, Settings)).

assert_settings(Result, ScenarioAttributes) ->
    ?assertMatch({ok, SettingsList} when is_list(SettingsList), Result),
    {ok, Settings} = Result,
    ?assertEqual(length(Settings), length(ScenarioAttributes)),
    [?assertEqual([{Name, Value}], proplists:lookup_all(Name, Settings)) ||
        {Name, _, Value, _} <- ScenarioAttributes].

correct_scenario_attributes() ->
    scenario_attributes(10000, 0, some_atom, <<"some_binary">>, <<"another_binary">>).

another_correct_scenario_attributes() ->
    scenario_attributes(20000, 3, another_atom, <<"yet_another_binary">>, <<"binary">>).

incorrect_scenario_attributes() ->
    scenario_attributes(0, -1, wrong_atom, an_atom, "string").

scenario_attributes(PosInt, NonNegInt, Atom, BitString1, BitString2) ->
    [
        {pos_int, "description", PosInt, positive_integer},
        {nonneg_int, "description", NonNegInt, nonnegative_integer},
        {atom, "description", Atom, [some_atom, another_atom]},
        {some_binary, "description", BitString1, binary},
        {another_binary, "description", BitString2, fun erlang:is_binary/1}
    ].

process_scenario_attributes(Attributes, Settings) ->
    Modules = [?MODULE, verification_module],
    {ok, Config} = amoc_config_scenario:process_scenario_attributes(Modules, Attributes),
    amoc_config_scenario:process_scenario_config(Config, Settings).

given_scenario_parameters_not_set(ScenarioAttributes) ->
    [unset_env(Name) || {Name, _, _, _} <- ScenarioAttributes].

given_scenario_parameters_set(ScenarioAttributes) ->
    [set_env(Name, Value) || {Name, _, Value, _} <- ScenarioAttributes].

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
