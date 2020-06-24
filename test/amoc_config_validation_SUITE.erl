-module(amoc_config_validation_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include("../src/amoc_config/amoc_config.hrl").

-compile(export_all).

-import(amoc_config_helper, [set_os_env/2,
                             unset_os_env/1,
                             set_app_env/3,
                             unset_app_env/2]).

-define(MOD, ct).
-define(APP, common_test).

all() ->
    [process_scenario_config_uses_default_values,
     process_scenario_config_shadows_default_values,
     process_scenario_config_returns_error_for_invalid_values,
     process_scenario_config_returns_preprocessed_value].

process_scenario_config_uses_default_values(_) ->
    ScenarioConfig = correct_scenario_config(),
    given_scenario_os_parameters_not_set(ScenarioConfig),
    given_scenario_app_parameters_not_set(ScenarioConfig),
    Result = amoc_config_validation:process_scenario_config(ScenarioConfig, []),
    ?assertEqual({ok, ScenarioConfig}, Result).

process_scenario_config_shadows_default_values(_) ->
    ScenarioConfig = correct_scenario_config(),
    AnotherScenarioConfig = another_correct_scenario_config(),
    YetAnotherScenarioConfig = yet_another_correct_scenario_config(),
    %% set app parameters
    given_scenario_os_parameters_not_set(ScenarioConfig),
    given_scenario_app_parameters_set(AnotherScenarioConfig),
    Result1 = amoc_config_validation:process_scenario_config(ScenarioConfig, []),
    ?assertEqual({ok, AnotherScenarioConfig}, Result1),
    %% set also os parameters
    given_scenario_os_parameters_set(YetAnotherScenarioConfig),
    Result2 = amoc_config_validation:process_scenario_config(ScenarioConfig, []),
    ?assertEqual({ok, YetAnotherScenarioConfig}, Result2),
    %% provide settings
    Settings = settings_from_scenario_config(ScenarioConfig),
    Result3 = amoc_config_validation:process_scenario_config(ScenarioConfig, Settings),
    ?assertEqual({ok, ScenarioConfig}, Result3),
    %% unset os parameters
    given_scenario_os_parameters_not_set(ScenarioConfig),
    Result4 = amoc_config_validation:process_scenario_config(ScenarioConfig, []),
    ?assertEqual({ok, AnotherScenarioConfig}, Result4),
    %%unset app parameters
    given_scenario_app_parameters_not_set(ScenarioConfig).

process_scenario_config_returns_error_for_invalid_values(_) ->
    VerificationFN = fun(_) -> {false, some_reason} end,
    IncorrectScenarioConfig =
    [#module_parameter{name = wrong_param, mod = ?MOD, value = any_value,
                       verification_fn = VerificationFN}
     | incorrect_scenario_config()],
    given_scenario_os_parameters_not_set(IncorrectScenarioConfig),
    given_scenario_app_parameters_not_set(IncorrectScenarioConfig),
    Result = amoc_config_validation:process_scenario_config(IncorrectScenarioConfig, []),
    ?assertEqual({error, parameters_verification_failed,
                  [{wrong_param, any_value, {verification_failed, some_reason}},
                   {some_int, wrong_int, verification_failed},
                   {some_atom, <<"wrong_atom">>, verification_failed},
                   {some_binary, "wrong_binary", verification_failed}]},
                 Result).

process_scenario_config_returns_preprocessed_value(_) ->
    ValidationFn = fun(_) -> {true, another_atom} end,
    PreprocessedParam = #module_parameter{name = preprocessed_param,
                                          mod = ?MOD, value = an_atom,
                                          verification_fn = ValidationFn},
    Result = amoc_config_validation:process_scenario_config([PreprocessedParam], []),
    ?assertEqual({ok, [PreprocessedParam#module_parameter{value = another_atom}]}, Result).

correct_scenario_config() ->
    scenario_configuration(1, some_atom, <<"some_binary">>).

another_correct_scenario_config() ->
    scenario_configuration(2, another_atom, <<"another_binary">>).

yet_another_correct_scenario_config() ->
    scenario_configuration(3, yet_another_atom, <<"yet_another_binary">>).

incorrect_scenario_config() ->
    scenario_configuration(wrong_int, <<"wrong_atom">>, "wrong_binary").

scenario_configuration(Int, Atom, Binary) ->
    [
        #module_parameter{name = some_int, mod = ?MOD, value = Int,
                          verification_fn = fun erlang:is_integer/1},
        #module_parameter{name = some_atom, mod = ?MOD, value = Atom,
                          verification_fn = fun erlang:is_atom/1},
        #module_parameter{name = some_binary, mod = ?MOD, value = Binary,
                          verification_fn = fun erlang:is_binary/1}
    ].

settings_from_scenario_config(ScenarioConfig) ->
    [{Name, Value} || #module_parameter{name = Name, value = Value} <- ScenarioConfig].

given_scenario_os_parameters_not_set(ScenarioConfig) ->
    [unset_os_env(Name) || #module_parameter{name = Name} <- ScenarioConfig].

given_scenario_os_parameters_set(ScenarioConfig) ->
    [set_os_env(Name, Value)
     || #module_parameter{name = Name, value = Value} <- ScenarioConfig].

given_scenario_app_parameters_not_set(ScenarioConfig) ->
    [unset_app_env(?APP, Name) || #module_parameter{name = Name} <- ScenarioConfig].

given_scenario_app_parameters_set(ScenarioConfig) ->
    [set_app_env(?APP, Name, Value)
     || #module_parameter{name = Name, value = Value} <- ScenarioConfig].
