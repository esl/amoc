%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config).

-export([get/1,
         get/2,
         fetch/1]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(atom()) -> any().
get(Name) ->
    get(Name, undefined).

-spec get(atom(), any()) -> any().
get(Name, Default) ->
    case os:getenv("AMOC_" ++ erlang:atom_to_list(Name)) of
        false ->
            application:get_env(amoc, Name, Default);
        Value ->
            parse_value(Value)
    end.       

-spec fetch(atom()) -> any().
fetch(Name) ->
    case os:getenv("AMOC_" ++ erlang:atom_to_list(Name)) of
        false ->
            case application:get_env(amoc, Name, undefined) of
                undefined ->
                    throw({key_not_found, Name});
                Value ->
                    Value
            end;
        Value ->
            parse_value(Value)
    end.
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec parse_value(string()) -> any().
parse_value(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
