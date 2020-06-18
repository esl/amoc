-module(amoc_config_env_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [parse_value_prop_test,
     config_os_env_test,
     config_app_env_test,
     config_os_env_shadows_app_env_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(amoc),
    Config.

end_per_suite(Config) ->
    Config.

parse_value_prop_test(_) ->
    RealAnyType = weighted_union([{1, map(any(), any())},
                                  {10, any()}]),
    ProperTest = ?FORALL(Value, RealAnyType,
                         amoc_config_env:parse_value(format_value(Value)) =:= {ok, Value}),
    ?assertEqual(true, proper:quickcheck(ProperTest,[quiet])).


config_os_env_test(_) ->
    ?assertEqual(undefined, get_env(some_variable)),
    ?assertEqual(default_value, get_env(some_variable, default_value)),
    set_os_env(some_variable, some_value),
    ?assertEqual(some_value, get_env(some_variable)),
    ?assertEqual(some_value, get_env(some_variable, default_value)),
    set_os_env(some_variable, another_value),
    ?assertEqual(another_value, get_env(some_variable)),
    ?assertEqual(another_value, get_env(some_variable, default_value)),
    unset_os_env(some_variable),
    ?assertEqual(undefined, get_env(some_variable)),
    ?assertEqual(default_value, get_env(some_variable, default_value)).

config_app_env_test(_) ->
    ?assertEqual(undefined, get_env(some_variable)),
    ?assertEqual(default_value, get_env(some_variable, default_value)),
    set_app_env(some_variable, some_value),
    ?assertEqual(some_value, get_env(some_variable)),
    ?assertEqual(some_value, get_env(some_variable, default_value)),
    set_app_env(some_variable, another_value),
    ?assertEqual(another_value, get_env(some_variable)),
    ?assertEqual(another_value, get_env(some_variable, default_value)),
    unset_app_env(some_variable),
    ?assertEqual(undefined, get_env(some_variable)),
    ?assertEqual(default_value, get_env(some_variable, default_value)).

config_os_env_shadows_app_env_test(_) ->
    ?assertEqual(undefined, get_env(some_variable)),
    ?assertEqual(default_value, get_env(some_variable, default_value)),
    set_app_env(some_variable, some_value),
    ?assertEqual(some_value, get_env(some_variable)),
    ?assertEqual(some_value, get_env(some_variable, default_value)),
    set_os_env(some_variable, another_value),
    ?assertEqual(another_value, get_env(some_variable)),
    ?assertEqual(another_value, get_env(some_variable, default_value)),
    unset_os_env(some_variable),
    ?assertEqual(some_value, get_env(some_variable)),
    ?assertEqual(some_value, get_env(some_variable, default_value)),
    unset_app_env(some_variable),
    ?assertEqual(undefined, get_env(some_variable)),
    ?assertEqual(default_value, get_env(some_variable, default_value)).


set_app_env(Name, Value) ->
    application:set_env(amoc, Name, Value).

unset_app_env(Name) ->
    application:unset_env(amoc, Name).

set_os_env(Name, Value) ->
    os:putenv(env_name(Name), format_value(Value)).

unset_os_env(Name) ->
    os:unsetenv(env_name(Name)).

env_name(Name) ->
    "AMOC_" ++ string:uppercase(erlang:atom_to_list(Name)).

get_env(Name) ->
    amoc_config_env:get(amoc, Name).

get_env(Name, Default) ->
    amoc_config_env:get(amoc, Name, Default).

format_value(Value) ->
    lists:flatten(io_lib:format("~tp", [Value])).
