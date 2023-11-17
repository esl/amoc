-module(amoc_config_scenario_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-import(amoc_config_helper, [set_app_env/3,
                             unset_app_env/2]).

-define(MOCK_MOD, mock_mod).

%% execution of update functions is done asynchronously
-define(UPDATE_TIMEOUT, 500).
%% execution of verification functions is done synchronously
-define(VERIFY_TIMEOUT, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% declaring required variables %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable(#{name => var0, description => "var0"}).
%% without overriding, verification  of var1 should fail.
-required_variable(#{name => var1, description => "var1",
                     verification => [unused_value]}).
-required_variable(#{name => var2, description => "var2", default_value => def2}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% overriding variables %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-override_variable([#{name => var1, description => "var1", default_value => val1,
                     verification => fun ?MOCK_MOD:verify_fun/1},
                    #{name => var2, description => "var2", default_value => val2,
                      verification => [new_val2, val2],
                      update => {?MOCK_MOD, update_mfa, 2}}]).
%% var3 is not declared with -required_variable(...), but it's fine.
-override_variable(#{name => var3, description => "var3", default_value => def3}).
-override_variable(#{name => var3, description => "var3", default_value => val3,
                     update => fun ?MOCK_MOD:update_fun/2,
                     verification => {?MOCK_MOD, verify_mfa, 1}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [parse_scenario_settings,
     implicit_variable_redefinition,
     crash_during_scenario_settings_parsing,
     invalid_module_attributes,
     invalid_settings,
     invalid_value,
     update_settings,
     update_just_one_parameter,
     update_parameters_with_the_same_values,
     update_settings_readonly,
     update_settings_invalid_value,
     update_settings_undef_param].

groups() ->
    [{update_settings, [], []}].

init_per_suite(Config) ->
    meck:new(?MOCK_MOD, [non_strict, no_link]),
    meck:expect(?MOCK_MOD, update_mfa, ['_', '_'], ok),
    meck:expect(?MOCK_MOD, verify_mfa, ['_'], true),
    meck:expect(?MOCK_MOD, update_fun, ['_', '_'], ok),
    meck:expect(?MOCK_MOD, verify_fun, ['_'], true),
    Config.

end_per_suite(Config) ->
    meck:unload(?MOCK_MOD),
    Config.

init_per_testcase(TC, Config) when TC =:= crash_during_scenario_settings_parsing;
                                   TC =:= invalid_module_attributes ->
    meck:new(amoc_config_attributes, [no_link]),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(TC, Config) when TC =:= crash_during_scenario_settings_parsing;
                                  TC =:= invalid_module_attributes ->
    meck:unload(amoc_config_attributes),
    meck:reset(?MOCK_MOD),
    Config;
end_per_testcase(_, Config) ->
    meck:reset(?MOCK_MOD),
    Config.

parse_scenario_settings(_) ->
    mock_ets_tables(),
    ets:insert(configurable_modules, {amoc_controller, configurable}),
    ScenarioSettings = [{interarrival, 500},
                        {var1, def1}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual(ok, Ret),
    %% check verification function calls
    meck:wait(?MOCK_MOD, verify_fun, [def1], ?VERIFY_TIMEOUT),
    meck:wait(?MOCK_MOD, verify_mfa, [val3], ?VERIFY_TIMEOUT),
    ?assertEqual(2, length(meck:history(?MOCK_MOD))),
    assert_no_update_calls(),
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
    set_initial_configuration(),

    %% update 2 parameters and check all values and
    %% verification/update function calls
    ScenarioSettings = [{var3, new_val3},
                        {var2, new_val2}],
    Ret = amoc_config_scenario:update_settings(ScenarioSettings),
    ?assertEqual(ok, Ret),
    %% updated variables
    ?assertEqual(new_val2, amoc_config:get(var2)),
    ?assertEqual(new_val3, amoc_config:get(var3)),
    %% unchanged variables
    ?assertEqual(undefined, amoc_config:get(var0)),
    ?assertEqual(val1, amoc_config:get(var1)),
    %% execution of update functions is done asynchronously
    %% and execution of verification functions is synchronous
    meck:wait(?MOCK_MOD, verify_mfa, [new_val3], ?VERIFY_TIMEOUT),
    meck:wait(?MOCK_MOD, update_fun, [var3, new_val3], ?UPDATE_TIMEOUT),
    meck:wait(?MOCK_MOD, update_mfa, [var2, new_val2], ?UPDATE_TIMEOUT),
    ?assertEqual(3, length(meck:history(?MOCK_MOD))).

update_just_one_parameter(_) ->
    set_initial_configuration(),

    %% update only 1 parameter and check that update
    %% function is call for that parameter
    ?assertEqual(ok, amoc_config_scenario:update_settings([{var2, new_val2}])),
    %% updated variables
    ?assertEqual(new_val2, amoc_config:get(var2)),
    %% execution of update functions is done asynchronously
    %% and execution of verification functions is synchronous
    meck:wait(?MOCK_MOD, update_mfa, [var2, new_val2], ?UPDATE_TIMEOUT),
    ?assertError(timeout, meck:wait(?MOCK_MOD, verify_fun, 2, ?VERIFY_TIMEOUT)),
    ?assertEqual(1, length(meck:history(?MOCK_MOD))),
    meck:reset(?MOCK_MOD),

    %% update another parameter and check that update
    %% function is call for it
    ?assertEqual(ok, amoc_config_scenario:update_settings([{var3, new_val3}])),
    %% updated variables
    ?assertEqual(new_val3, amoc_config:get(var3)),
    %% execution of update functions is done asynchronously
    %% and execution of verification functions is synchronous
    meck:wait(?MOCK_MOD, verify_mfa, [new_val3], ?VERIFY_TIMEOUT),
    meck:wait(?MOCK_MOD, update_fun, [var3, new_val3], ?UPDATE_TIMEOUT),
    ?assertEqual(2, length(meck:history(?MOCK_MOD))),
    meck:reset(?MOCK_MOD),

    %% run amoc_config_scenario:update_settings/2 with empty list
    ?assertEqual(ok, amoc_config_scenario:update_settings([])),
    assert_no_update_calls(),
    assert_no_verify_calls().

update_parameters_with_the_same_values(_) ->
    set_initial_configuration(),

    %% update 2 parameters with inital values, check that values are unchanged
    %% and no verification/update functions called
    ScenarioSettings = [{var3, val3},
                        {var2, val2}],
    ?assertEqual(ok, amoc_config_scenario:update_settings(ScenarioSettings)),
    %% updated variables
    ?assertEqual(val2, amoc_config:get(var2)),
    ?assertEqual(val3, amoc_config:get(var3)),

    assert_no_update_calls(),
    assert_no_verify_calls(),

    %% update 2 parameters but only one with the new value, check the value is
    %% changed and no verification/update functions are called only for that
    %% parameter
    ScenarioSettings2 = [{var3, new_val3},
                        {var2, val2}],
    ?assertEqual(ok, amoc_config_scenario:update_settings(ScenarioSettings2)),
    %% updated variables
    ?assertEqual(val2, amoc_config:get(var2)),
    ?assertEqual(new_val3, amoc_config:get(var3)),

    meck:wait(?MOCK_MOD, verify_mfa, [new_val3], ?VERIFY_TIMEOUT),
    meck:wait(?MOCK_MOD, update_fun, [var3, new_val3], ?UPDATE_TIMEOUT),
    ?assertEqual(2, length(meck:history(?MOCK_MOD))).

update_settings_readonly(_) ->
    set_initial_configuration(),
    Table = ets:tab2list(amoc_config),
    %% updating readonly parameters
    ScenarioSettings = [{var0, val0},
                        {var1, val1}, %% the same value as set initially
                        {var3, val3}],
    ReadOnlyRet = amoc_config_scenario:update_settings(ScenarioSettings),
    ?assertEqual({error, readonly_parameters,
                  [{var0, ?MODULE},
                   {var1, ?MODULE}]},
                 ReadOnlyRet),
    is_equal_list(Table, ets:tab2list(amoc_config)),
    assert_no_update_calls(),
    assert_no_verify_calls().

update_settings_invalid_value(_) ->
    set_initial_configuration(),
    Table = ets:tab2list(amoc_config),
    %% invalid value
    ScenarioSettings2 = [{var3, new_val3},
                         {var2, invalid_val2}],
    InvalidValueRet = amoc_config_scenario:update_settings(ScenarioSettings2),
    ?assertEqual({error, parameters_verification_failed,
                  [{var2, invalid_val2,
                    {verification_failed, {not_one_of, [new_val2, val2]}}}]},
                 InvalidValueRet),
    is_equal_list(Table, ets:tab2list(amoc_config)),
    assert_no_update_calls().

update_settings_undef_param(_) ->
    set_initial_configuration(),
    Table = ets:tab2list(amoc_config),
    %% reset initial verification calls.
    meck:reset(?MOCK_MOD),
    %% adding undefined parameter in settings
    ScenarioSettings3 = [{var3, new_val3},
                         {invalid_var2, val2}],
    UndefParamRet = amoc_config_scenario:update_settings(ScenarioSettings3),
    ?assertEqual({error, undefined_parameters, [invalid_var2]}, UndefParamRet),
    is_equal_list(Table, ets:tab2list(amoc_config)),
    assert_no_update_calls(),
    assert_no_verify_calls().


implicit_variable_redefinition(_) ->
    mock_ets_tables(),
    ets:insert(configurable_modules, {?MODULE, configurable}),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual({error, parameter_overriding, {var0, ?MODULE, ?MODULE}}, Ret),
    assert_no_update_calls(),
    assert_no_verify_calls().

invalid_settings(_) ->
    mock_ets_tables(),
    ScenarioSettings = [{invalid_var1, invalid_value},
                        {var2, new_val2},
                        {invalid_var2, invalid_value}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual({error, undefined_parameters, [invalid_var2, invalid_var1]}, Ret),
    assert_no_update_calls(),
    assert_no_verify_calls().

invalid_value(_) ->
    mock_ets_tables(),
    ScenarioSettings = [{var2, invalid_value}],
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, ScenarioSettings),
    ?assertEqual({error, parameters_verification_failed,
                  [{var2, invalid_value,
                    {verification_failed, {not_one_of, [new_val2, val2]}}}]},
                 Ret),
    %% check verification function calls, verification
    meck:wait(?MOCK_MOD, verify_fun, [val1], ?VERIFY_TIMEOUT),
    meck:wait(?MOCK_MOD, verify_mfa, [val3], ?VERIFY_TIMEOUT),
    assert_no_update_calls(),
    ?assertEqual(2, length(meck:history(?MOCK_MOD))).

crash_during_scenario_settings_parsing(_) ->
    mock_ets_tables(),
    ExceptionValue = some_exception,
    meck:expect(amoc_config_attributes, get_module_configuration,
                fun(_, _) -> meck:exception(throw, ExceptionValue) end),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertMatch({error, pipeline_action_crashed, {throw, some_exception, _}}, Ret),
    assert_no_update_calls(),
    assert_no_verify_calls().

invalid_module_attributes(_) ->
    mock_ets_tables(),
    ErrorValue = {error, some_error_type, some_error_reason},
    meck:expect(amoc_config_attributes, get_module_configuration,
                fun(_, _) -> ErrorValue end),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual(ErrorValue, Ret),
    assert_no_update_calls(),
    assert_no_verify_calls().

mock_ets_tables() ->
    EtsOptions = [named_table, protected, {read_concurrency, true}],
    configurable_modules = ets:new(configurable_modules, EtsOptions),
    amoc_config_utils:create_amoc_config_ets().

assert_no_update_calls() ->
    ?assertError(timeout, meck:wait(?MOCK_MOD, update_mfa, 2, ?UPDATE_TIMEOUT)),
    %% no need to wait for timeout twice
    ?assertError(timeout, meck:wait(?MOCK_MOD, update_fun, 2, 0)).

assert_no_verify_calls() ->
    ?assertError(timeout, meck:wait(?MOCK_MOD, verify_mfa, 1, ?VERIFY_TIMEOUT)),
    ?assertError(timeout, meck:wait(?MOCK_MOD, verify_fun, 1, ?VERIFY_TIMEOUT)).

is_equal_list(List1, List2) ->
    ?assertEqual(lists:sort(List1), lists:sort(List2)).

set_initial_configuration() ->
    mock_ets_tables(),
    Ret = amoc_config_scenario:parse_scenario_settings(?MODULE, []),
    ?assertEqual(ok, Ret),
    %% check verification function calls
    % ct:pal("meck:history(?MOCK_MOD) = ~p", [meck:history(?MOCK_MOD)]),
    meck:wait(?MOCK_MOD, verify_fun, [val1], ?VERIFY_TIMEOUT),
    meck:wait(?MOCK_MOD, verify_mfa, [val3], ?VERIFY_TIMEOUT),
    %% reset initial verification calls.
    assert_no_update_calls(),
    ?assertEqual(2, length(meck:history(?MOCK_MOD))),
    meck:reset(?MOCK_MOD).
