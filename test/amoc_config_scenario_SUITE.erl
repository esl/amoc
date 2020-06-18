-module(amoc_config_scenario_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-import(amoc_config_helper, [set_app_env/3,
                             unset_app_env/2]).
-define(APP, common_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable({var0, "var0"}).
-required_variable({var1, "var1"}).
-required_variable({var2, "var2", def2}).
-override_variable({var2, "var2", val2, [def2, val2]}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [parse_scenario_settings,
     implicit_variable_redefinition,
     invalid_verification_module,
     invalid_parametrised_module,
     invalid_module_attributes,
     invalid_value].

parse_scenario_settings(_) ->
    mock_ets_tables(),
    ets:insert(amoc_scenarios, {amoc_controller, parametrised}),
    ScenarioSettings = [{undefined_var, some_value},
                        {interarrival, 500},
                        {var1, def1}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual(ok, Ret),
    %% undefined variable provided in settings
    ?assertThrow({invalid_setting, undefined_var}, amoc_config:get(undefined_var)),
    %% undefined variable
    ?assertThrow({invalid_setting, another_undefined_var},
                 amoc_config:get(another_undefined_var)),
    %% undefined value
    ?assertEqual(undefined, amoc_config:get(var0)),
    ?assertEqual(some_value, amoc_config:get(var0, some_value)),
    %% defined (in settings) value
    ?assertEqual(def1, amoc_config:get(var1)),
    ?assertEqual(def1, amoc_config:get(var1, some_value)),
    %% overwritten variable
    ?assertEqual(val2, amoc_config:get(var2)),
    %% parametrised module variable (defined in amoc_controller)
    ?assertEqual(500, amoc_config:get(interarrival)).

implicit_variable_redefinition(_) ->
    mock_ets_tables(),
    ets:insert(amoc_scenarios, {?MODULE, parametrised}),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual({error, parameter_overriding, {var0, ?MODULE, ?MODULE}}, Ret).

invalid_value(_) ->
    mock_ets_tables(),
    ScenarioSettings = [{var2, invalid_value}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual({error, parameters_verification_failed,
                  [{var2, invalid_value,
                    {verification_failed, {not_one_of, [def2, val2]}}}]},
                 Ret).

invalid_verification_module(_) ->
    mock_ets_tables(),
    set_app_env(?APP, config_verification_modules, [invalid_module_name]),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    unset_app_env(?APP, config_verification_modules),
    ?assertEqual({error, invalid_verification_module,
                  [{invalid_module_name, nofile}]},
                 Ret).

invalid_parametrised_module(_) ->
    mock_ets_tables(),
    ets:insert(amoc_scenarios, {invalid_module_name, parametrised}),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual({error, invalid_module, invalid_module_name}, Ret).

invalid_module_attributes(_) ->
    mock_ets_tables(),
    meck:new(amoc_config_attributes, [non_strict, no_link]),
    ErrorValue = {error, some_error_type, some_error_reason},
    meck:expect(amoc_config_attributes, get_module_configuration,
                fun(_, _, _) -> ErrorValue end),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual(ErrorValue, Ret),
    meck:unload().

mock_ets_tables() ->
    EtsOptions = [named_table, protected, {read_concurrency, true}],
    amoc_config = ets:new(amoc_config, EtsOptions),
    amoc_scenarios = ets:new(amoc_scenarios, EtsOptions).