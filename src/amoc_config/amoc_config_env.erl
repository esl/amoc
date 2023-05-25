%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% This module can be used directly only for the readonly env init parameters.
%% do not use it for the scenarios/helpers configuration, amoc_config module
%% must be used instead! This allows to provide configuration via REST API in
%% a JSON format
%%==============================================================================
-module(amoc_config_env).

-export([get/1, get/2, parse_value/1, format/2]).

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(amoc_config:name()) -> amoc_config:value().
get(Name) ->
    get(Name, undefined).

-spec get(amoc_config:name(), amoc_config:value()) -> amoc_config:value().
get(Name, Default) when is_atom(Name) ->
    get_os_env(Name, Default).

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

-spec format(any(), binary) -> binary();
            (any(), string)  -> string().
format(Value, binary) ->
    list_to_binary(format(Value, string));
format(Value, string) ->
    lists:flatten(io_lib:format("~tp", [Value])).

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
