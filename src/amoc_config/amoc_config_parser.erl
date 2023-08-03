%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% This module implements the default parser for the amoc_config_env module
%%==============================================================================
-module(amoc_config_parser).
-behaviour(amoc_config_env).

-export([parse_value/1]).

-ifdef(TEST).
%% exported for testing only
-export([format/2]).
-else.
-ignore_xref([format/2]).
-dialyzer({nowarn_function, [format/2]}).
-endif.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

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
