-module(amoc_config_scenario_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-import(amoc_config_helper, [set_app_env/3,
                             unset_app_env/2]).

-define(MOCK_MOD, mock_mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable(#{name => var0, description => "var0"}).
-required_variable(#{name => var1, description => "var1"}).
-required_variable(#{name => var2, description => "var2", default_value => def2}).
-override_variable(#{name => var2, description => "var2", default_value => val2,
                     verification => [def2, val2], update => fun ?MODULE:update_fn/2}).
-override_variable(#{name => var3, description => "var3", default_value => def3,
                     update => fun ?MODULE:update_fn/2}).

update_fn(Name, Value) -> apply(?MOCK_MOD, update, [Name, Value]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [parse_scenario_settings,
     implicit_variable_redefinition,
     crash_during_scenario_settings_parsing,
     invalid_module_attributes,
     invalid_settings,
     invalid_value,
     {group, update_settings}].

groups() ->
    [{update_settings, [], [update_settings,
                            update_settings_readonly,
                            update_settings_invalid_value,
                            update_settings_undef_param]}].

init_per_group(update_settings,Config) ->
    meck:new(?MOCK_MOD, [non_strict, no_link]),
    meck:expect(?MOCK_MOD, update, ['_', '_'], ok),
    Config.

end_per_group(update_settings,Config) ->
    meck:unload(),
    Config.

parse_scenario_settings(_) ->
    mock_ets_tables(),
    ets:insert(amoc_scenarios, {amoc_controller, configurable}),
    ScenarioSettings = [{interarrival, 500},
                        {var1, def1}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual(ok, Ret),
    %% undefined variable
    ?assertThrow({invalid_setting, undefined_var}, amoc_config:get(undefined_var)),
    %% undefined value
    ?assertEqual(undefined, amoc_config:get(var0)),
    ?assertEqual(some_value, amoc_config:get(var0, some_value)),
    %% defined (in settings) value
    ?assertEqual(def1, amoc_config:get(var1)),
    ?assertEqual(def1, amoc_config:get(var1, some_value)),
    %% overwritten variable
    ?assertEqual(val2, amoc_config:get(var2)),
    %% configurable module variable (defined in amoc_controller)
    ?assertEqual(500, amoc_config:get(interarrival)).

update_settings(_) ->
    mock_ets_tables(),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual(ok, Ret),
    ScenarioSettings = [{var3, val3},
                        {var2, def2}],
    Ret2 = amoc_config_scenario:update_settings(ScenarioSettings),
    ?assertEqual(ok, Ret2),
    %% updated variables
    ?assertEqual(def2, amoc_config:get(var2)),
    ?assertEqual(val3, amoc_config:get(var3)),
    %% unchanged variables
    ?assertEqual(undefined, amoc_config:get(var0)),
    ?assertEqual(undefined, amoc_config:get(var1)),
    [meck:wait(?MOCK_MOD, update, [Name, Value], 500)
     || {Name, Value} <- ScenarioSettings],
    meck:reset(?MOCK_MOD).

update_settings_readonly(_) ->
    mock_ets_tables(),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    Table = ets:tab2list(amoc_config),
    %% updating readonly parameters
    ScenarioSettings = [{var0, val0},
                        {var1, def1},
                        {var3, val3}],
    ReadOnlyRet = amoc_config_scenario:update_settings(ScenarioSettings),
    ?assertEqual({error, readonly_parameters,
                  [{var0, ?MODULE},
                   {var1, ?MODULE}]},
                 ReadOnlyRet),
    isEqualList(Table, ets:tab2list(amoc_config)),
    ?assertError(timeout, meck:wait(?MOCK_MOD, update, 2, 500)).

update_settings_invalid_value(_) ->
    mock_ets_tables(),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    Table = ets:tab2list(amoc_config),
    %% invalid value
    ScenarioSettings2 = [{var3, val3},
                         {var2, invalid_val2}],
    InvalidValueRet = amoc_config_scenario:update_settings(ScenarioSettings2),
    ?assertEqual({error, parameters_verification_failed,
                  [{var2, invalid_val2,
                    {verification_failed, {not_one_of, [def2, val2]}}}]},
                 InvalidValueRet),
    isEqualList(Table, ets:tab2list(amoc_config)),
    ?assertError(timeout, meck:wait(?MOCK_MOD, update, 2, 500)).

update_settings_undef_param(_) ->
    mock_ets_tables(),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    Table = ets:tab2list(amoc_config),
    %% undefined parameter
    ScenarioSettings3 = [{var3, val3},
                         {invalid_var2, val2}],
    UndefParamRet = amoc_config_scenario:update_settings(ScenarioSettings3),
    ?assertEqual({error, undefined_parameters, [invalid_var2]}, UndefParamRet),
    isEqualList(Table, ets:tab2list(amoc_config)),
    ?assertError(timeout, meck:wait(?MOCK_MOD, update, 2, 500)).

implicit_variable_redefinition(_) ->
    mock_ets_tables(),
    ets:insert(amoc_scenarios, {?MODULE, configurable}),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual({error, parameter_overriding, {var0, ?MODULE, ?MODULE}}, Ret).

invalid_settings(_) ->
    mock_ets_tables(),
    ScenarioSettings = [{invalid_var1, invalid_value},
                        {var2, def2},
                        {invalid_var2, invalid_value}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual({error, undefined_parameters, [invalid_var2, invalid_var1]}, Ret).

invalid_value(_) ->
    mock_ets_tables(),
    ScenarioSettings = [{var2, invalid_value}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual({error, parameters_verification_failed,
                  [{var2, invalid_value,
                    {verification_failed, {not_one_of, [def2, val2]}}}]},
                 Ret).


crash_during_scenario_settings_parsing(_) ->
    mock_ets_tables(),
    meck:new(amoc_config_attributes, []),
    ExceptionValue = some_exception,
    meck:expect(amoc_config_attributes, get_module_configuration,
                fun(_, _) -> meck:exception(throw, ExceptionValue) end),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertMatch({error, pipeline_action_crashed, {throw, some_exception, _}},
                 Ret),
    meck:unload().

invalid_module_attributes(_) ->
    mock_ets_tables(),
    meck:new(amoc_config_attributes, []),
    ErrorValue = {error, some_error_type, some_error_reason},
    meck:expect(amoc_config_attributes, get_module_configuration,
                fun(_, _) -> ErrorValue end),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual(ErrorValue, Ret),
    meck:unload().

mock_ets_tables() ->
    EtsOptions = [named_table, protected, {read_concurrency, true}],
    amoc_scenarios = ets:new(amoc_scenarios, EtsOptions),
    amoc_config_utils:create_amoc_config_ets().

isEqualList(List1, List2) ->
    ?assertEqual(lists:sort(List1), lists:sort(List2)).
