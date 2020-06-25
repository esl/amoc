%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% this module can be used directly only for the readonly env init parameters.
%% do not use it for the scenarios/helpers configuration, amoc_config module
%% must be used instead! this allows to provide configuration via REST API in
%% a JSON format
%%==============================================================================
-module(amoc_config_env).

-export([get/2, get/3, parse_value/1, find_all_vars/1]).

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(atom(), amoc_config:name()) -> amoc_config:value().
get(AppName, Name) ->
    get(AppName, Name, undefined).

-spec get(atom(), amoc_config:name(), amoc_config:value()) -> amoc_config:value().
get(AppName, Name, Default) when is_atom(Name) ->
    DefValue = application:get_env(AppName, Name, Default),
    get_os_env(Name, DefValue).

-spec parse_value(string() | binary()) -> {ok, amoc_config:value()} | {error, any()}.
parse_value(Binary) when is_binary(Binary) ->
    parse_value(binary_to_list(Binary));
parse_value(String) when is_list(String) ->
    try
        {ok, Tokens, _} = erl_scan:string(String ++ "."),
        {ok, _} = erl_parse:parse_term(Tokens)
    catch
        _:E -> {error, E}
    end.

-spec find_all_vars(atom()) -> [any()].
find_all_vars(Name) ->
    AllValues = [application:get_env(App, Name)
                 || {App, _, _} <- application:loaded_applications()],
    [Value || {ok, Value} <- AllValues].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_os_env(amoc_config:name(), amoc_config:value()) -> amoc_config:value().
get_os_env(Name, Default) ->
    EnvName = os_env_name(Name),
    Value = os:getenv(EnvName),
    case parse_value(Value, Default) of
        {ok, Term} -> Term;
        {error, _} ->
            ?LOG_ERROR("cannot parse $~p value \"~p\", using default one", [EnvName, Value]),
            Default
    end.

-spec os_env_name(amoc_config:name()) -> string().
os_env_name(Name) when is_atom(Name) ->
    "AMOC_" ++ string:uppercase(erlang:atom_to_list(Name)).

-spec parse_value(string() | false, any()) -> {ok, amoc_config:value()} | {error, any()}.
parse_value(false, Default) -> {ok, Default};
parse_value("", Default)    -> {ok, Default};
parse_value(String, _) ->
    parse_value(String).
