-module(config_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [config_osenv_test,
     config_appenv_test,
     config_none_test,
     config_osappenv_test,
     config_osenv_dynamic_test].

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
