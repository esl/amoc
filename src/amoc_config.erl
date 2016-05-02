%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config).

-export([start/0,
         set/2,
         get/1,
         get/2,
         fetch/1]).

%% For tests
-export([set_env_variables/2]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec start() -> ok.
start() ->
    ok = set_env_variables(os:getenv()).

-spec set(atom(), any()) -> ok.
set(Name, Value) ->
    ok = application:set_env(amoc, Name, Value).

-spec get(atom()) -> any().
get(Name) ->
    get(Name, undefined).

-spec get(atom(), any()) -> any().
get(Name, Default) ->
    application:get_env(amoc, Name, Default).

-spec fetch(atom()) -> any().
fetch(Name) ->
    case application:get_env(amoc, Name) of
        {ok, Value} -> Value;
        undefined -> throw({key_not_found, Name})
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec set_env_variables([string()]) -> ok.
set_env_variables(EnvVars) ->
    set_env_variables(EnvVars, fun(_) -> true end).

-spec set_env_variables([string()], fun((string()) -> boolean())) -> ok.
set_env_variables(EnvVars, Filter) ->
    Vars = parse_variables(EnvVars),
    [ ok = set(Name, Value) || {Name, Value} <- Vars, Filter(Name) ],
    ok.

-spec parse_variables([string()]) -> [{atom(), any()}].
parse_variables(EnvVars) ->
    Vars = [ re:split(Var, "=", [{return, list}, {parts, 2}])
             || Var <- EnvVars ],
    %% list_to_atom should be safe here, as we are in control of env variables
    [ {list_to_atom(Name), parse_value(Value)} ||
      ["AMOC_" ++ Name, Value] <- Vars ].

-spec parse_value(string()) -> any().
parse_value(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
