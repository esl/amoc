%% @copyright 2024 Erlang Solutions Ltd.
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
%%
%% Runs on the controller process and spans a `[amoc, scenario, init, _]' telemetry event.
-spec init(amoc:scenario()) -> {ok, state()} | {error, Reason :: term()}.
init(Scenario) ->
    case verify_exports_callbacks(Scenario) of
        true ->
            apply_safely(Scenario, init, [], #{scenario => Scenario});
        false ->
            {error, invalid_scenario}
    end.

verify_exports_callbacks(Scenario) ->
    erlang:function_exported(Scenario, init, 0)
    andalso erlang:function_exported(Scenario, start, 2)
    orelse erlang:function_exported(Scenario, start, 1).

%% @doc Applies the `Scenario:terminate/0,1' callback
%%
%% `Scenario:terminate/0' and `Scenario:terminate/1' callbacks are optional.
%% If the scenario module exports both functions, `Scenario:terminate/1' is used.
%%
%% Runs on the controller process and spans a `[amoc, scenario, terminate, _]' telemetry event.
-spec terminate(amoc:scenario(), state()) -> ok | {ok, any()} | {error, Reason :: term()}.
terminate(Scenario, State) ->
    Metadata = #{scenario => Scenario, state => State},
    case {erlang:function_exported(Scenario, terminate, 1),
          erlang:function_exported(Scenario, terminate, 0)} of
        {true, _} ->
            %% since we ignore Scenario:terminate/1 return value
            %% we can use apply_safely/3 function
            apply_safely(Scenario, terminate, [State], Metadata);
        {_, true} ->
            %% since we ignore Scenario:terminate/0 return value
            %% we can use apply_safely/3 function
            apply_safely(Scenario, terminate, [], Metadata);
        _ ->
            ok
    end.

%% @doc Applies the `Scenario:start/1,2' callback
%%
%% Either `Scenario:start/1' or `Scenario:start/2' must be exported from the behaviour module.
%% if scenario module exports both functions, `Scenario:start/2' is used.
%%
%% Runs on the user process and spans a `[amoc, scenario, user, _]' telemetry event.
-spec start(amoc:scenario(), user_id(), state()) -> term().
start(Scenario, Id, State) ->
    Metadata = #{scenario => Scenario, state => State, user_id => Id},
    Span = case {erlang:function_exported(Scenario, start, 2),
                 erlang:function_exported(Scenario, start, 1)} of
               {true, _} ->
                   fun() ->
                           Ret = Scenario:start(Id, State),
                           {Ret, Metadata#{return => Ret}}
                   end;
               {_, true} ->
                   fun() ->
                           Ret = Scenario:start(Id),
                           {Ret, Metadata#{return => Ret}}
                   end;
               {false, false} ->
                   exit("the scenario module must export either start/2 or start/1 function")
           end,
    telemetry:span([amoc, scenario, start], Metadata, Span).

%% ------------------------------------------------------------------
%% internal functions
%% ------------------------------------------------------------------

-spec apply_safely(atom(), atom(), [term()], map()) -> {ok | error, term()}.
apply_safely(M, F, A, Metadata) ->
    Span = fun() ->
                   Ret = erlang:apply(M, F, A),
                   {Ret, Metadata#{return => Ret}}
           end,
    try telemetry:span([amoc, scenario, F], Metadata, Span) of
        {ok, RetVal} -> {ok, RetVal};
        {error, Error} -> {error, Error};
        Result -> {ok, Result}
    catch
        Class:Exception:Stacktrace ->
            {error, {Class, Exception, Stacktrace}}
    end.
