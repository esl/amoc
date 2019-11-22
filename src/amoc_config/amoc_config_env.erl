%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config_env).

-export([get/1, get/2, parse_value/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(EUNIT).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(amoc_config:name()) -> amoc_config:value().
get(Name) ->
    get(Name, undefined).

-spec get(amoc_config:name(), amoc_config:value()) -> amoc_config:value().
get(Name, Default) when is_atom(Name) ->
    DefValue = application:get_env(amoc, Name, Default),
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

-ifdef(EUNIT).
parse_value_test() ->
    RealAnyType = weighted_union([{1, map(any(), any())},
                                  {10, any()}]),
    ProperTest = ?FORALL(Value,
                         RealAnyType,
                         begin
                             StringValue = lists:flatten(io_lib:format("~p", [Value])),
                             %% ?debugMsg(StringValue),
                             parse_value(StringValue) =:= {ok, Value}
                         end),
    ?assertEqual(true, proper:quickcheck(ProperTest)).
-endif.