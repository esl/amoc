%% @see amoc_config
%% @see amoc_config_env
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc This module defines a behaviour to parse values as extracted from environment variables.
%% This module implements the default parser for the `amoc_config_env' module
%% @end
-module(amoc_config_parser).
-behaviour(amoc_config_env).

-export([parse_value/1]).

%% format/2 is exported for testing purposes
%% it is also re-used by amoc-arsenal
-export([format/2]).

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
