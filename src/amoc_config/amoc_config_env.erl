%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% This module can be used directly only for the readonly env init parameters.
%% do not use it for the scenarios/helpers configuration, amoc_config module
%% must be used instead! This allows to provide configuration via REST API in
%% a JSON format
%%==============================================================================
-module(amoc_config_env).

-export([get/1, get/2, parse_value/1, format/2]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(atom()) -> any().
get(Name) ->
    get(Name, undefined).

-spec get(atom(), any()) -> any().
get(Name, Default) when is_atom(Name) ->
    get_os_env(Name, Default).

-spec parse_value(string() | binary()) -> {ok, any()} | {error, any()}.
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
-spec get_os_env(atom(), any()) -> any().
get_os_env(Name, Default) ->
    EnvName = os_env_name(Name),
    Value = os:getenv(EnvName),
    case parse_value(Value, Default) of
        {ok, Term} -> Term;
        {error, _} ->
            lager:error("cannot parse $~p value \"~p\", using default one", [EnvName, Value]),
            Default
    end.

-spec os_env_name(atom()) -> string().
os_env_name(Name) when is_atom(Name) ->
    "AMOC_" ++ string:to_upper(erlang:atom_to_list(Name)).

-spec parse_value(string() | false, any()) -> {ok, any()} | {error, any()}.
parse_value(false, Default) -> {ok, Default};
parse_value("", Default)    -> {ok, Default};
parse_value(String, _) ->
    parse_value(String).
