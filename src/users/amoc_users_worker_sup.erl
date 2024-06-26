%% @private
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Supervisor-like gen_server with some tracking responsibilities over the users
%%
%% We want to keep consistent track of all users running globally by running special code upon a
%% children's death. Standard solutions don't cut it because:
%%  - A supervisor doesn't expose callbacks on user termination
%%  - Implementing code on the user process before it dies risks inconsistencies if it is killed
%%  More improvements that could be made would be to distribute the supervision tree like ranch did,
%%  see https://stressgrid.com/blog/100k_cps_with_elixir/
%% @end
-module(amoc_users_worker_sup).

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start_child/4, stop_child/2, start_children/4, stop_children/3, terminate_all_children/1]).

-export([get_all_children/1]).

-record(state, {
          index :: non_neg_integer(),
          table :: ets:table(),
          tasks = #{} :: #{reference() := pid()}
         }).
-type state() :: #state{}.

-define(SHUTDOWN_TIMEOUT, 2000). %% 2 seconds

%% @private
-spec start_link(non_neg_integer()) -> gen_server:start_ret().
start_link(N) ->
    gen_server:start_link(?MODULE, N, []).

-spec start_child(pid(), amoc:scenario(), amoc_scenario:user_id(), any()) -> ok.
start_child(Sup, Scenario, Id, ScenarioState) ->
    gen_server:cast(Sup, {start_child, Scenario, Id, ScenarioState}).

-spec start_children(pid(), amoc:scenario(), [amoc_scenario:user_id()], any()) -> ok.
start_children(Sup, Scenario, UserIds, ScenarioState) ->
    gen_server:cast(Sup, {start_children, Scenario, UserIds, ScenarioState}).

-spec stop_children(pid(), non_neg_integer(), boolean()) -> ok.
stop_children(Sup, Count, Force) ->
    gen_server:cast(Sup, {stop_children, Count, Force}).

-spec terminate_all_children(pid()) -> any().
terminate_all_children(Sup) ->
    gen_server:cast(Sup, terminate_all_children).

-spec stop_child(pid(), boolean()) -> ok.
stop_child(Pid, false) ->
    exit(Pid, shutdown),
    ok;
stop_child(Pid, true) ->
    spawn(shutdown_and_kill_after_timeout_fun(Pid)),
    ok.

-spec get_all_children(pid()) -> [{pid(), amoc_scenario:user_id()}].
get_all_children(Sup) ->
    gen_server:call(Sup, get_all_children, infinity).

%% @private
-spec init(non_neg_integer()) -> {ok, state()}.
init(N) ->
    process_flag(trap_exit, true),
    Name = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(N)),
    Table = ets:new(Name, [ordered_set, protected, named_table]),
    {ok, #state{index = N, table = Table}}.

%% @private
-spec handle_call(any(), any(), state()) -> {reply, term(), state()}.
handle_call(get_all_children, _From, #state{table = Table} = State) ->
    Children = ets:tab2list(Table),
    {reply, Children, State}.

%% @private
-spec handle_cast(Request, state()) -> {noreply, state()} when
      Request :: {start_child, amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:state()} |
             {start_children, amoc:scenario(), [amoc_scenario:user_id()], amoc_scenario:state()} |
             {stop_children, non_neg_integer(), boolean()} |
             terminate_all_children.
handle_cast({start_child, Scenario, Id, ScenarioState}, State) ->
    do_start_child(Scenario, Id, ScenarioState, State),
    {noreply, State};
handle_cast({start_children, Scenario, Ids, ScenarioState}, State) ->
    [ do_start_child(Scenario, Id, ScenarioState, State) || Id <- Ids],
    {noreply, State};
handle_cast({stop_children, 0, _}, State) ->
    {noreply, State};
handle_cast({stop_children, Int, ForceRemove}, #state{table = Table} = State) ->
    Pids = case ets:match_object(Table, '$1', Int) of
               '$end_of_table' ->
                   [];
               {Objects, _} ->
                   [Pid || {Pid, _Id} <- Objects]
           end,
    NewState = maybe_track_task_to_stop_my_children(State, Pids, ForceRemove),
    {noreply, NewState};
handle_cast(terminate_all_children, State) ->
    NewState = do_terminate_all_my_children(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Request, state()) -> {noreply, state()} when
      Request :: {child_up, pid(), amoc_scenario:user_id()} |
                 {'DOWN', reference(), process, pid(), term()} |
                 {'EXIT', pid(), term()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{tasks = Tasks} = State) ->
    {noreply, State#state{tasks = maps:remove(Ref, Tasks)}};
handle_info({'EXIT', Pid, _Reason}, #state{index = N, table = Table} = State) ->
    handle_down_user(Table, Pid, N),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> state().
terminate(_Reason, State) ->
    do_terminate_all_my_children(State).

%% Helpers

-spec do_start_child(module(), amoc_scenario:user_id(), term(), state()) -> any().
do_start_child(Scenario, Id, ScenarioState, #state{index = N, table = Table}) ->
    case amoc_user:start_link(Scenario, Id, ScenarioState) of
        {ok, Pid} ->
            handle_up_user(Table, Pid, Id, N);
        _ ->
            ok
    end.

-spec handle_up_user(ets:table(), pid(), amoc_scenario:user_id(), non_neg_integer()) -> any().
handle_up_user(Table, Pid, Id, SupNum) ->
    ets:insert(Table, {Pid, Id}),
    amoc_users_sup:incr_no_of_users(SupNum).

-spec handle_down_user(ets:table(), pid(), non_neg_integer()) -> ok.
handle_down_user(Table, Pid, SupNum) ->
    ets:delete(Table, Pid),
    amoc_users_sup:decr_no_of_users(SupNum).

%% @doc Stop a list of users in parallel.
%% We don't want to ever block the supervisor on `timer:sleep/1' so we spawn that async.
%% However we don't want free processes roaming around, we want monitoring that can be traced.
-spec maybe_track_task_to_stop_my_children(state(), [pid()], boolean()) -> state().
maybe_track_task_to_stop_my_children(State, [], _) ->
    State;
maybe_track_task_to_stop_my_children(State, Pids, false) ->
    [ exit(Pid, shutdown) || Pid <- Pids ],
    State;
maybe_track_task_to_stop_my_children(#state{tasks = Tasks} = State, Pids, true) ->
    {Pid, Ref} = spawn_monitor(shutdown_and_kill_after_timeout_fun(Pids)),
    State#state{tasks = Tasks#{Pid => Ref}}.

-spec shutdown_and_kill_after_timeout_fun(pid() | [pid()]) -> fun(() -> term()).
shutdown_and_kill_after_timeout_fun([_ | _] = Pids) ->
    fun() ->
            [ exit(Pid, shutdown) || Pid <- Pids ],
            timer:sleep(?SHUTDOWN_TIMEOUT),
            [ exit(Pid, kill) || Pid <- Pids ]
    end;
shutdown_and_kill_after_timeout_fun(Pid) when is_pid(Pid) ->
    fun() ->
            exit(Pid, shutdown),
            timer:sleep(?SHUTDOWN_TIMEOUT),
            exit(Pid, kill)
    end.

-spec do_terminate_all_my_children(state()) -> state().
do_terminate_all_my_children(#state{table = Table} = State) ->
    Match = ets:match_object(Table, '$1', 200),
    do_terminate_all_my_children(State, Match).

%% ets:continuation/0 type is unfortunately not exported from the ets module.
-spec do_terminate_all_my_children(state(), {[tuple()], term()} | '$end_of_table') -> state().
do_terminate_all_my_children(State, {Objects, Continuation}) ->
    Pids = [Pid || {Pid, _Id} <- Objects],
    NewState = maybe_track_task_to_stop_my_children(State, Pids, true),
    Match = ets:match_object(Continuation),
    do_terminate_all_my_children(NewState, Match);
do_terminate_all_my_children(State, '$end_of_table') ->
    State.
