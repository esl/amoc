%% @copyright 2023 Erlang Solutions Ltd.
%% @doc Wrapper around the defined scenario
-module(amoc_scenario).

-export([init/1, terminate/2, start/3]).

%%-------------------------------------------------------------------------
%% behaviour definition
%%-------------------------------------------------------------------------
-export_type([user_id/0, state/0]).

-type user_id() :: pos_integer(). %% Unique integer ID of every actor spawned
-type state() :: term(). %% The state of the scenario as returned by `init/0'

-callback init() -> ok | {ok, state()} | {error, Reason :: term()}.
-callback start(user_id(), state()) -> any().
%% `start/2' is preferred over `start/1'. At least one of them is required.
-callback start(user_id()) -> any().
-callback terminate(state()) -> any().
%% `terminate/1' is preferred over `terminate/0'
-callback terminate() -> any().

-optional_callbacks([start/1, start/2]).
-optional_callbacks([terminate/0, terminate/1]).

%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------

%% @doc Applies the `Scenario:init/0' callback
-spec init(amoc:scenario()) -> {ok, state()} | {error, Reason :: term()}.
init(Scenario) ->
    apply_safely(Scenario, init, []).

%% @doc Applies the `Scenario:terminate/0,1' callback
%%
%% `Scenario:terminate/0' and `Scenario:terminate/1' callbacks are optional.
%% If the scenario module exports both functions, `Scenario:terminate/1' is used.
-spec terminate(amoc:scenario(), state()) -> ok | {ok, any()} | {error, Reason :: term()}.
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

%% @doc Applies the `Scenario:start/1,2' callback
%%
%% Either `Scenario:start/1' or `Scenario:start/2' must be exported from the behaviour module.
%% if scenario module exports both functions, `Scenario:start/2' is used.
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
