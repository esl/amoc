%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_controller).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(USERS_TABLE, amoc_users).

-required_variable(#{name => interarrival, value => 50, verification => positive_integer,
                     description => "a delay between creating the processes for two "
                                    "consecutive users (ms, def: 50ms)"}).

-record(state, {scenario :: amoc:scenario() | undefined,
                no_of_users = 0 :: user_count(),
                last_user_id = 0 :: last_user_id(),
                status = idle :: idle | running | terminating | finished |
                                 {error, any()} | disabled,
                scenario_state :: any(),
                create_users = [] :: [amoc_scenario:user_id()],
                tref :: timer:tref() | undefined}).

-type state() :: #state{}.
-type handle_call_res() :: ok | {ok, term()} | {error, term()}.
-type amoc_status() :: {idle, [LoadedScenario :: amoc:scenario()]} |
                       {running, amoc:scenario(), user_count(), last_user_id()} |
                       {terminating, amoc:scenario()} |
                       {finished, amoc:scenario()} |
                       {error, any()} |
                       disabled. %% amoc_controller is disabled for the master node

-type user_count() :: non_neg_integer().
-type last_user_id() :: non_neg_integer().
-type interarrival() :: non_neg_integer().

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% Types Exports
%% ------------------------------------------------------------------

-export_type([amoc_status/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         start_scenario/2,
         stop_scenario/0,
         update_settings/1,
         add_users/2,
         remove_users/2,
         get_status/0,
         disable/0]).

%% ------------------------------------------------------------------
%% Parameters verification functions
%% ------------------------------------------------------------------
-export([positive_integer/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_scenario(amoc:scenario(), amoc_config:settings()) ->
    ok | {error, term()}.
start_scenario(Scenario, Settings) ->
    case amoc_scenario:does_scenario_exist(Scenario) of
        true ->
            gen_server:call(?SERVER, {start_scenario, Scenario, Settings});
        false ->
            {error, {no_such_scenario, Scenario}}
    end.

-spec stop_scenario() -> ok | {error, term()}.
stop_scenario() ->
    gen_server:call(?SERVER, stop_scenario).

-spec update_settings(amoc_config:settings()) -> ok | {error, term()}.
update_settings(Settings) ->
    gen_server:call(?SERVER, {update_settings, Settings}).

-spec add_users(amoc_scenario:user_id(), amoc_scenario:user_id()) ->
    ok | {error, term()}.
add_users(StartId, EndID) ->
    %% adding the exact range of the users
    gen_server:call(?SERVER, {add, StartId, EndID}).

-spec remove_users(user_count(), boolean()) -> {ok, user_count()}.
remove_users(Count, ForceRemove) ->
    %% trying to remove Count users, this action is async!!!
    gen_server:call(?SERVER, {remove, Count, ForceRemove}).

-spec get_status() -> amoc_status().
get_status() ->
    {ok, Status} = gen_server:call(?SERVER, get_status),
    Status.

-spec disable() -> ok | {error, term()}.
disable() ->
    gen_server:call(?SERVER, disable).

positive_integer(Interarrival) ->
    is_integer(Interarrival) andalso Interarrival > 0.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(priority, max),
    start_tables(),
    {ok, #state{}}.

-spec handle_call(any(), any(), state()) -> {reply, handle_call_res(), state()}.
handle_call({start_scenario, Scenario, Settings}, _From, State) ->
    {RetValue, NewState} = handle_start_scenario(Scenario, Settings, State),
    {reply, RetValue, NewState};
handle_call(stop_scenario, _From, State) ->
    {RetValue, NewState} = handle_stop_scenario(State),
    {reply, RetValue, NewState};
handle_call({update_settings, Settings}, _From, State) ->
    RetValue = handle_update_settings(Settings),
    {reply, RetValue, State};
handle_call({add, StartId, EndID}, _From, State) ->
    {RetValue, NewState} = handle_add(StartId, EndID, State),
    {reply, RetValue, NewState};
handle_call({remove, Count, ForceRemove}, _From, State) ->
    RetValue = handle_remove(Count, ForceRemove, State),
    {reply, RetValue, State};
handle_call(get_status, _From, State) ->
    RetValue = handle_status(State),
    {reply, {ok, RetValue}, State};
handle_call(disable, _From, State) ->
    {RetValue, NewState} = handle_disable(State),
    {reply, RetValue, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(start_user, State) ->
    NewSate = handle_start_user(State),
    {noreply, NewSate};
handle_info({'DOWN', _, process, Pid, _}, State) ->
    NewSate = handle_stop_user(Pid, State),
    {noreply, NewSate};
handle_info(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% internal functions
%% ------------------------------------------------------------------
-spec handle_start_scenario(module(), amoc_config:settings(), state()) ->
    {handle_call_res(), state()}.
handle_start_scenario(Scenario, Settings, #state{status = idle} = State) ->
    case init_scenario(Scenario, Settings) of
        {ok, ScenarioState} ->
            NewState = State#state{scenario       = Scenario,
                                   scenario_state = ScenarioState,
                                   status         = running},
            {ok, NewState};
        {error, _} = Error ->
            NewState = State#state{scenario = Scenario, status = Error},
            {Error, NewState}
    end;
handle_start_scenario(_Scenario, _Settings, #state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_stop_scenario(state()) -> {handle_call_res(), state()}.
handle_stop_scenario(#state{status = running} = State) ->
    terminate_all_users(),
    {ok, State#state{status = terminating}};
handle_stop_scenario(#state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_update_settings(amoc_config:settings()) -> handle_call_res().
handle_update_settings(Settings) ->
    case amoc_config_scenario:update_settings(Settings) of
        ok -> ok;
        {error, Type, Reason} -> {error, {Type, Reason}}
    end.

-spec handle_add(amoc_scenario:user_id(), amoc_scenario:user_id(), state()) ->
    {handle_call_res(), state()}.
handle_add(StartId, EndId, #state{last_user_id = LastId,
                                  create_users = ScheduledUsers,
                                  status       = running,
                                  tref         = TRef} = State) when StartId =< EndId,
                                                                     LastId < StartId ->
    NewUsers = lists:seq(StartId, EndId),
    NewScheduledUsers = lists:append(ScheduledUsers, NewUsers),
    NewTRef = maybe_start_timer(TRef),
    {ok, State#state{create_users = NewScheduledUsers, tref = NewTRef,
                     last_user_id = EndId}};
handle_add(_StartId, _EndId, #state{status = running} = State) ->
    {{error, invalid_range}, State};
handle_add(_Scenario, _Settings, #state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_remove(user_count(), boolean(), state()) -> handle_call_res().
handle_remove(Count, ForceRemove, #state{status = running}) ->
    Pids = case ets:match_object(?USERS_TABLE, '$1', Count) of
               {Objects, _} -> [Pid || {_Id, Pid} <- Objects];
               '$end_of_table' -> []
           end,
    amoc_users_sup:stop_children(Pids, ForceRemove),
    {ok, length(Pids)};
handle_remove(_Count, _ForceRemove, #state{status = Status}) ->
    {error, {invalid_status, Status}}.

-spec handle_status(state()) -> amoc_status().
handle_status(#state{status = idle}) ->
    {idle, amoc_scenario:list_scenario_modules()};
handle_status(#state{status = running, scenario = Scenario,
                     no_of_users = N, last_user_id = LastId}) ->
    {running, Scenario, N, LastId};
handle_status(#state{status = terminating, scenario = Scenario}) ->
    {terminating, Scenario};
handle_status(#state{status = finished, scenario = Scenario}) ->
    {finished, Scenario};
handle_status(#state{status = Status}) ->
    Status. %% {error, Reason} or disabled.

-spec handle_disable(state()) -> {handle_call_res(), state()}.
handle_disable(#state{status = idle} = State) ->
    {ok, State#state{status = disabled}};
handle_disable(#state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_start_user(state()) -> state().
handle_start_user(#state{create_users   = [UserId | T],
                         no_of_users    = N,
                         scenario       = Scenario,
                         scenario_state = ScenarioState} = State) ->
    start_user(Scenario, UserId, ScenarioState),
    State#state{create_users = T, no_of_users = N + 1};
handle_start_user(#state{create_users = [], tref = TRef} = State) ->
    State#state{tref = maybe_stop_timer(TRef)}.

-spec handle_stop_user(pid(), state()) -> state().
handle_stop_user(Pid, State) ->
    case ets:match(?USERS_TABLE, {'$1', Pid}, 1) of
        {[[UserId]], _} ->
            ets:delete(?USERS_TABLE, UserId),
            dec_no_of_users(State);
        _ ->
            State
    end.

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------
-spec start_tables() -> ok.
start_tables() -> %% ETS creation
    ?USERS_TABLE = ets:new(?USERS_TABLE, [named_table,
                                          ordered_set,
                                          protected,
                                          {read_concurrency, true}]),
    amoc_config_utils:create_amoc_config_ets(),
    ok.

-spec init_scenario(amoc:scenario(), amoc_config:settings()) ->
    {ok | error, any()}.
init_scenario(Scenario, Settings) ->
    case amoc_config_scenario:parse_scenario_settings(Scenario, Settings) of
        ok -> apply_safely(Scenario, init, []);
        {error, Type, Reason} -> {error, {Type, Reason}}
    end.

-spec maybe_start_timer(timer:tref()|undefined) -> timer:tref().
maybe_start_timer(undefined) ->
    {ok, TRef} = timer:send_interval(interarrival(), start_user),
    TRef;
maybe_start_timer(TRef) -> TRef.

-spec maybe_stop_timer(timer:tref()|undefined) -> undefined.
maybe_stop_timer(undefined) ->
    undefined;
maybe_stop_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    undefined.

-spec start_user(amoc:scenario(), amoc_scenario:user_id(), any()) -> ok.
start_user(Scenario, Id, ScenarioState) ->
    {ok, Pid} = supervisor:start_child(amoc_users_sup, [Scenario, Id, ScenarioState]),
    ets:insert(?USERS_TABLE, {Id, Pid}),
    erlang:monitor(process, Pid),
    ok.

-spec terminate_all_users() -> any().
terminate_all_users() ->
    %stop all the users
    Match = ets:match_object(?USERS_TABLE, '$1', 200),
    terminate_all_users(Match).

-spec terminate_all_users({tuple(), ets:continuation()} | '$end_of_table') -> ok.
terminate_all_users({Objects, Continuation}) ->
    Pids = [Pid || {_Id, Pid} <- Objects],
    amoc_users_sup:stop_children(Pids, true),
    Match = ets:match_object(Continuation),
    terminate_all_users(Match);
terminate_all_users('$end_of_table') -> ok.

-spec dec_no_of_users(state()) -> state().
dec_no_of_users(#state{scenario    = Scenario, scenario_state = ScenarioState,
                       no_of_users = 1, status = terminating} = State) ->
    apply_safely(Scenario, terminate, [ScenarioState]),
    State#state{no_of_users = 0, status = finished};
dec_no_of_users(#state{no_of_users = N} = State) ->
    State#state{no_of_users = N - 1}.

-spec interarrival() -> interarrival().
interarrival() ->
    amoc_config:get(interarrival).

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
