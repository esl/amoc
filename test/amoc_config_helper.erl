-module(amoc_config_helper).

-compile(export_all).

set_app_env(App, Name, Value) ->
    application:set_env(App, Name, Value).

unset_app_env(App, Name) ->
    application:unset_env(App, Name).

set_os_env(Name, Value) ->
    os:putenv(env_name(Name), format_value(Value)).

unset_os_env(Name) ->
    os:unsetenv(env_name(Name)).

env_name(Name) ->
    "AMOC_" ++ string:uppercase(erlang:atom_to_list(Name)).

get_env(App, Name) ->
    amoc_config_env:get(App, Name).

get_env(App, Name, Default) ->
    amoc_config_env:get(App, Name, Default).

format_value(Value) ->
    lists:flatten(io_lib:format("~tp", [Value])).
