-module(config_SUITE).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [config_osenv_test,
     config_appenv_test,
     config_none_test,
     config_osappenv_test,
     config_osenv_dynamic_test,
     parse_scenario_settings_returns_map_if_all_params_are_valid,
     parse_scenario_settings_uses_default_value_if_a_param_is_not_set,
     parse_scenario_settings_shadows_default_value].

config_osenv_test(_Config) ->
    given_osenv_set([{"AMOC_interarrival", "100"},
                     {"AMOC_something", "[{a,b,c}, 9, 8, {7}]"}]),
    given_amoc_started(),
    ?assertEqual(100, amoc_config:get(interarrival)),
    ?assertEqual(60000, amoc_config:get(repeat_interval)),
    ?assertEqual(something, amoc_config:get(anything, something)),
    ?assertEqual([{a,b,c},9,8,{7}], amoc_config:get(something)).

given_amoc_started() ->
    {ok, _} = application:ensure_all_started(amoc).

given_osenv_set(Envs) ->
    [ true = os:putenv(Name, Value) || {Name, Value} <- Envs ].

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
    os:putenv("AMOC_foo", "bar"),
    application:set_env(amoc, foo, baz),
    %% then
    ?assertEqual(bar, amoc_config:get(foo)).

config_osenv_dynamic_test(_) ->
    %% given
    given_amoc_started(),
    %% when
    os:putenv("AMOC_foo", "bar"),
    %% then
    ?assertEqual(bar, amoc_config:get(foo)),
    %% when
    os:putenv("AMOC_foo", "baz"),
    %% then
    ?assertEqual(baz, amoc_config:get(foo)).

parse_scenario_settings_returns_map_if_all_params_are_valid(_) ->
    given_amoc_started(),
    ScenarioParams = correct_scenario_params(),
    given_scenario_parameters_set(ScenarioParams),
    Result = amoc_config:parse_scenario_settings(ScenarioParams),
    ?assertMatch({ok, SettingsMap} when is_map(SettingsMap), Result).

parse_scenario_settings_uses_default_value_if_a_param_is_not_set(_) ->
    given_amoc_started(),
    ScenarioParams = correct_scenario_params(),
    given_scenario_parameters_set(ScenarioParams),
    SettingsWithNotSetParam = [{not_set_param, 'NOT_SET_SCENARIO_PARAM',  1234, none} | ScenarioParams],
    {ok, Settings} = amoc_config:parse_scenario_settings(SettingsWithNotSetParam),
    ?assertMatch({ok, 1234}, amoc_config:get_scenario_parameter(not_set_param, Settings)).

parse_scenario_settings_shadows_default_value(_) ->
    given_amoc_started(),
    ScenarioParams = correct_scenario_params(),
    given_scenario_parameters_set(ScenarioParams),
    [{Name, Var, OriginalValue, _} | Rest] = ScenarioParams,
    NewRandomValue = base64:encode(crypto:strong_rand_bytes(5)),
    SettingsWithChangedDefault = [{Name, Var, NewRandomValue, none} | Rest],
    {ok, Settings} = amoc_config:parse_scenario_settings(SettingsWithChangedDefault),
    ?assertMatch({ok, OriginalValue}, amoc_config:get_scenario_parameter(Name, Settings)).



correct_scenario_params() ->
    [
     {iq_timeout,         'IQ_TIMEOUT',                  10000, positive_integer},
     {coordinator_delay,  'COORDINATOR_DELAY',               0, nonnegative_integer},
     {activation_policy,  'ACTIVATION_POLICY',       all_nodes, [all_nodes, n_nodes]},
     {mim_host,           'MIM_HOST',          <<"localhost">>, bitstring}
    ].

given_scenario_parameters_set(ScenarioParams) ->
    Params = [{Name, Value} || {_, Name, Value, _} <- ScenarioParams],
    application:set_env([{amoc, Params}]).
