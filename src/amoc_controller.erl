%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_controller).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(INTERARRIVAL_DEFAULT, 50).
-define(INTERARRIVAL,
        application:get_env(amoc, interarrival, ?INTERARRIVAL_DEFAULT)).

-define(TABLE, amoc_users).

-record(state, {scenario,
                scenario_state,
                nodes,
                node_id}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         do/3,
         do/7,
         add/1,
         add/2,
         remove/1,
         remove/2,
         remove/3,
         users/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

do(Scenario, Start, End) ->
    gen_server:call(?SERVER, {do, Scenario, Start, End}).

do(Node, Scenario, Start, End, Nodes, NodeId, Opts) ->
    gen_server:call({?SERVER, Node}, {do, Scenario, Start, End, Nodes, NodeId, Opts}).

add(Count) ->
    gen_server:cast(?SERVER, {add, Count}).

add(Node, Count) ->
    gen_server:cast({?SERVER, Node}, {add, Count}).

remove(Count) ->
    remove(Count, []).

remove(Count, Opts) ->
    gen_server:cast(?SERVER, {remove, Count, Opts}).

remove(Node, Count, Opts) ->
    gen_server:cast({?SERVER, Node}, {remove, Count, Opts}).

users() ->
    gen_server:call(?SERVER, users).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    process_flag(priority, max),
    State = #state{scenario = undefined},
    {ok, State}.

handle_call({do, Scenario, Start, End}, _From, State) ->
    handle_local_do(Scenario, Start, End, State);
handle_call({do, Scenario, Start, End, Nodes, NodeId, Opts}, _From, State) ->
    handle_dist_do(Scenario, Start, End, Nodes, NodeId, Opts, State);
handle_call(users, _From, State) ->
    Reply = [{count, ets:info(?TABLE, size)},
             {last, ets:last(?TABLE)}],
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Count}, State) ->
    handle_add(Count, State),
    {noreply, State};
handle_cast({remove, Count, Opts}, State) ->
    handle_remove(Count, Opts, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_scenario, Scenario, UserIds, ScenarioState}, State) ->
    start_scenario(Scenario, UserIds, ScenarioState),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% callbacks handlers
%% ------------------------------------------------------------------
handle_add(_Count, #state{scenario=undefined}) ->
    lager:error("add users invoked, but no scenario defined");
handle_add(Count, #state{scenario=Scenario,
                         scenario_state=State,
                         nodes = Nodes,
                         node_id = NodeId}) when
      is_integer(Count), Count > 0 ->
    Last = case ets:last(?TABLE) of
               '$end_of_table' -> 0;
               Other -> Other
           end,
    UserIds = node_userids(Last+1, Last+Count, Nodes, NodeId),
    start_users(Scenario, UserIds, State).

handle_remove(_Count, _Opts, #state{scenario=undefined}) ->
    lager:error("remove users invoked, but no scenario defined");
handle_remove(Count, Opts, _State) when
      is_integer(Count), Count > 0 ->
    ForceRemove = proplists:get_value(force, Opts, false),
    Users = last_users(Count),
    stop_users(Users, ForceRemove).

handle_local_do(Scenario, Start, End, State) ->
    handle_do(Scenario, lists:seq(Start, End), State).

handle_dist_do(Scenario, Start, End, Nodes, NodeId, Opts, State) ->
    UserIds = node_userids(Start, End, Nodes, NodeId),
    State1 = State#state{nodes = Nodes,
                         node_id = NodeId},
    handle_do(Scenario, UserIds, State1).

handle_do(Scenario, UserIds, State) ->
    case code:ensure_loaded(Scenario) of
        {module, Scenario} ->
            ScenarioState = init_scenario(Scenario),
            self() ! {start_scenario, Scenario, UserIds, ScenarioState},
            State1 = State#state{scenario = Scenario,
                                 scenario_state = ScenarioState},
            {reply, ok, State1};
        Error ->
            lager:error("scenario module ~p cannot be found, reason: ~p",
                        [Scenario, Error]),
            {reply, {error, Error}, State}
    end.

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------
start_scenario(Scenario, UserIds, State) ->
    Start = lists:min(UserIds),
    End = lists:max(UserIds),
    Length = erlang:length(UserIds),
    lager:info("starting scenario begin_id=~p, end_id=~p, length=~p",
               [Start, End, Length]),
    start_users(Scenario, UserIds, State).

init_scenario(Scenario) ->
    case erlang:function_exported(Scenario, init, 0) of
        true ->
            Scenario:init();
        false ->
            skip
    end.

start_users(Scenario, UserIds, State) ->
    [ start_user(Scenario, Id, State) || Id <- UserIds ].

start_user(Scenario, Id, State) ->
    R = supervisor:start_child(amoc_users_sup, [Scenario, Id, State]),
    timer:sleep(?INTERARRIVAL),
    R.

stop_users(Users, _ForceRemove=true) ->
    [ begin
          ets:delete(?TABLE, Id),
          exit(Pid, shutdown)
      end || {Id, Pid} <- Users ];
stop_users(Users, _ForceRemove=false) ->
    [ Pid ! stop || {_, Pid} <- Users ].

last_users(Count) ->
    [ User || [User] <- last_users(Count, ets:last(?TABLE), []) ].

last_users(0, _, Acc) ->
    Acc;
last_users(_, '$end_of_table', Acc) ->
    Acc;
last_users(Count, Current, Acc) ->
    Prev = ets:prev(?TABLE, Current),
    last_users(Count-1, Prev, [ ets:lookup(?TABLE, Current) | Acc ]).

node_userids(Start, End, undefined, _) ->
    lists:seq(Start, End);
node_userids(Start, End, Nodes, NodeId) ->
    F = fun(Id) when Id rem Nodes + 1 =:= NodeId ->
                true;
           (_) ->
                false
        end,
    lists:filter(F, lists:seq(Start, End)).
