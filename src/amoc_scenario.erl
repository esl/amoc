%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).

-export([init/1, terminate/2, start/3]).

%%-------------------------------------------------------------------------
%% behaviour definition
%%-------------------------------------------------------------------------
-export_type([user_id/0, state/0]).

-type user_id() :: pos_integer().
-type state() :: any().

-callback init() -> {ok, state()} | ok | {error, Reason :: term()}.
-callback start(user_id(), state()) -> any().
-callback start(user_id()) -> any().
-callback terminate(state()) -> any().

%% either start/1 or start/2 must be exported from the behaviour module
-optional_callbacks([start/1, start/2]).
-optional_callbacks([terminate/1]).

%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------
-spec init(amoc:scenario()) -> {ok, state()} | {error, Reason :: term()}.
init(Scenario) ->
    apply_safely(Scenario, init, []).

-spec terminate(amoc:scenario(), state()) -> {ok, any()} | {error, Reason :: term()}.
terminate(Scenario, State) ->
    apply_safely(Scenario, terminate, [State]).

-spec start(amoc:scenario(), user_id(), state()) -> any().
start(Scenario, Id, State) ->
   case erlang:function_exported(Scenario, start, 2) of
              true ->
                  Scenario:start(Id, State);
              false ->
                  Scenario:start(Id)
    end.

%% ------------------------------------------------------------------
%% internal functions
%% ------------------------------------------------------------------
-spec apply_safely(atom(), atom(), [term()]) -> {ok | error, term()}.
apply_safely(M, F, A) ->
    try erlang:apply(M, F, A) of
        {ok, RetVal} -> {ok, RetVal};
        {error, Error} -> {error, Error};
        Result -> {ok, Result}
    catch
        Class:Exception:Stacktrace ->
            {error, {Class, Exception, Stacktrace}}
    end.
