-module(amoc_config_env_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(amoc_config_helper, [format_value/1,
                             get_env/2,
                             get_env/3,
                             set_os_env/2,
                             unset_os_env/1,
                             set_app_env/3,
                             unset_app_env/2]).

-compile(export_all).

-define(APP, common_test).

all() ->
    [parse_value_prop_test,
     config_os_env_test,
     config_app_env_test,
     config_os_env_shadows_app_env_test].

parse_value_prop_test(_) ->
    RealAnyType = weighted_union([{1, map(any(), any())},
                                  {10, any()}]),
    ProperTest = ?FORALL(Value, RealAnyType,
                         amoc_config_env:parse_value(format_value(Value)) =:= {ok, Value}),
    ?assertEqual(true, proper:quickcheck(ProperTest,[quiet])).

config_os_env_test(_) ->
    ?assertEqual(undefined, get_env(?APP, some_variable)),
    ?assertEqual(default_value, get_env(?APP, some_variable, default_value)),
    set_os_env(some_variable, some_value),
    ?assertEqual(some_value, get_env(?APP, some_variable)),
    ?assertEqual(some_value, get_env(?APP, some_variable, default_value)),
    set_os_env(some_variable, another_value),
    ?assertEqual(another_value, get_env(?APP, some_variable)),
    ?assertEqual(another_value, get_env(?APP, some_variable, default_value)),
    unset_os_env(some_variable),
    ?assertEqual(undefined, get_env(?APP, some_variable)),
    ?assertEqual(default_value, get_env(?APP, some_variable, default_value)).

config_app_env_test(_) ->
    ?assertEqual(undefined, get_env(?APP, some_variable)),
    ?assertEqual(default_value, get_env(?APP, some_variable, default_value)),
    set_app_env(?APP, some_variable, some_value),
    ?assertEqual(some_value, get_env(?APP, some_variable)),
    ?assertEqual(some_value, get_env(?APP, some_variable, default_value)),
    set_app_env(?APP, some_variable, another_value),
    ?assertEqual(another_value, get_env(?APP, some_variable)),
    ?assertEqual(another_value, get_env(?APP, some_variable, default_value)),
    unset_app_env(?APP, some_variable),
    ?assertEqual(undefined, get_env(?APP, some_variable)),
    ?assertEqual(default_value, get_env(?APP, some_variable, default_value)).

config_os_env_shadows_app_env_test(_) ->
    ?assertEqual(undefined, get_env(?APP, some_variable)),
    ?assertEqual(default_value, get_env(?APP, some_variable, default_value)),
    set_app_env(?APP, some_variable, some_value),
    ?assertEqual(some_value, get_env(?APP, some_variable)),
    ?assertEqual(some_value, get_env(?APP, some_variable, default_value)),
    set_os_env(some_variable, another_value),
    ?assertEqual(another_value, get_env(?APP, some_variable)),
    ?assertEqual(another_value, get_env(?APP, some_variable, default_value)),
    unset_os_env(some_variable),
    ?assertEqual(some_value, get_env(?APP, some_variable)),
    ?assertEqual(some_value, get_env(?APP, some_variable, default_value)),
    unset_app_env(?APP, some_variable),
    ?assertEqual(undefined, get_env(?APP, some_variable)),
    ?assertEqual(default_value, get_env(?APP, some_variable, default_value)).
