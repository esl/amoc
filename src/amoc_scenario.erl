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
-callback terminate() -> any().

%% either start/1 or start/2 must be exported from the behavior module.
%% if scenario module exports both functions, start/2 is used.
-optional_callbacks([start/1, start/2]).

%% terminate/0,1 callbacks are optional.
%% if scenario module exports both functions, terminate/1 is used.
-optional_callbacks([terminate/0, terminate/1]).

%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------
-spec init(amoc:scenario()) -> {ok, state()} | {error, Reason :: term()}.
init(Scenario) ->
    apply_safely(Scenario, init, []).

-spec terminate(amoc:scenario(), state()) -> {ok, any()} | {error, Reason :: term()}.
terminate(Scenario, State) ->
    case {erlang:function_exported(Scenario, terminate, 1),
          erlang:function_exported(Scenario, terminate, 0)} of
        {true, _} ->
            %% since we ignore Scenario:terminate/1 return value
            %% we can use apply_safely/3 function
            apply_safely(Scenario, terminate, [State]);
        {_, true} ->
            %% since we ignore Scenario:terminate/0 return value
            %% we can use apply_safely/3 function
            apply_safely(Scenario, terminate, []);
        _ ->
            ok
    end.

-spec start(amoc:scenario(), user_id(), state()) -> any().
start(Scenario, Id, State) ->
    case {erlang:function_exported(Scenario, start, 2),
          erlang:function_exported(Scenario, start, 1)} of
        {true, _} ->
            Scenario:start(Id, State);
        {_, true} ->
            Scenario:start(Id);
        {false, false} ->
            error("the scenario module must export either start/2 or start/1 function")
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
