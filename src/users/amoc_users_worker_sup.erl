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

-export([start_children/4, child_up/2, stop_children/3]).

-record(state, {
          index :: non_neg_integer(),
          waiting = #{} :: #{pid() := reference()},
          tasks = #{} :: #{reference() := pid()}
         }).
-type state() :: #state{}.

-define(SHUTDOWN_TIMEOUT, 2000). %% 2 seconds

%% @private
-spec start_link(non_neg_integer()) -> gen_server:start_ret().
start_link(N) ->
    gen_server:start_link(?MODULE, N, []).

-spec child_up(pid(), amoc_scenario:user_id()) -> any().
child_up(Sup, Id) ->
    gen_server:cast(Sup, {child_up, self(), Id}).

-spec start_children(pid(), amoc:scenario(), [amoc_scenario:user_id()], any()) -> ok.
start_children(Sup, Scenario, UserIds, ScenarioState) ->
    gen_server:cast(Sup, {start_children, Scenario, UserIds, ScenarioState}).

-spec stop_children(pid(), [pid()], boolean()) -> ok.
stop_children(Sup, Pids, Force) ->
    gen_server:cast(Sup, {stop_children, Pids, Force}).

%% @private
-spec init(non_neg_integer()) -> {ok, state()}.
init(N) ->
    process_flag(trap_exit, true),
    {ok, #state{index = N}}.

%% @private
-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%% @private
-spec handle_cast(Request, state()) -> {noreply, state()} when Request ::
    {start_children, amoc:scenario(), [amoc_scenario:user_id()], amoc_scenario:state()} |
    {stop_children, [pid()], boolean()}.
handle_cast({start_children, Scenario, Ids, ScenarioState}, State) ->
    NewState = do_start_children(State, Scenario, Ids, ScenarioState),
    {noreply, NewState};
handle_cast({child_up, Pid, Id}, State) ->
    NewState = handle_up_user(State, Pid, Id),
    {noreply, NewState};
handle_cast({stop_children, Pids, ForceRemove}, State) ->
    NewState = do_stop_children(State, Pids, ForceRemove),
    {noreply, NewState}.

%% @private
-spec handle_info(Request, state()) -> {noreply, state()} when
      Request :: {child_up, pid(), amoc_scenario:user_id()} |
                 {'DOWN', reference(), process, pid(), term()} |
                 {'EXIT', pid(), term()}.
handle_info({'EXIT', Pid, _Reason}, State) ->
    NewState = handle_down_user(State, Pid),
    {noreply, NewState};
handle_info({'DOWN', Ref, process, Pid, _Reason},
            #state{waiting = Waiting, tasks = Tasks} = State) ->
    case {Waiting, Tasks} of
        {#{Pid := Ref}, _} ->
            flush_exit(Pid),
            {noreply, State#state{waiting = maps:remove(Pid, Waiting)}};
        {_, #{Ref := Pid}} ->
            {noreply, State#state{tasks = maps:remove(Ref, Tasks)}};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> any().
terminate(_Reason, #state{waiting = Waiting, tasks = Tasks}) ->
    {TaskPid, TaskRef} = spawn_monitor(shutdown_and_kill_after_timeout_fun(maps:keys(Waiting))),
    maps:foreach(fun flush_down/2, Tasks#{TaskRef => TaskPid}).

%% Helpers
-spec do_start_children(state(), amoc:scenario(), [amoc_scenario:user_id()], term()) -> state().
do_start_children(#state{waiting = Waiting} = State, Scenario, Ids, ScenarioState) ->
    Fun = fun(Id, Acc) ->
                  {Pid, Ref} = amoc_user:start_link(Scenario, Id, ScenarioState),
                  [{Pid, Ref} | Acc]
          end,
    NewWaiting = lists:foldl(Fun, [], Ids),
    State#state{waiting = maps:merge(Waiting, maps:from_list(NewWaiting))}.

-spec handle_up_user(state(), pid(), amoc_scenario:user_id()) -> state().
handle_up_user(#state{index = SupNum, waiting = Waiting} = State, Pid, Id) ->
    amoc_users_sup:handle_up_user(SupNum, Pid, Id),
    State#state{waiting = maps:remove(Pid, Waiting)}.

-spec handle_down_user(state(), pid()) -> state().
handle_down_user(#state{index = SupNum} = State, Pid) ->
    amoc_users_sup:handle_down_user(SupNum, Pid),
    State.

-spec flush_exit(pid()) -> ok.
flush_exit(Pid) ->
    unlink(Pid),
    receive {'EXIT', Pid, _} -> ok after 0 -> ok end.

-spec flush_down(reference(), pid()) -> ok.
flush_down(Ref, Pid) ->
    receive {'DOWN', Ref, process, Pid, _} -> ok after 0 -> ok end.

%% @doc Stop a list of users in parallel.
%% We don't want to ever block the supervisor on `timer:sleep/1' so we spawn that async.
%% However we don't want free processes roaming around, we want monitoring that can be traced.
-spec do_stop_children(state(), [pid()], boolean()) -> state().
do_stop_children(State, Pids, false) ->
    [ exit(Pid, shutdown) || Pid <- Pids ],
    State;
do_stop_children(#state{tasks = Tasks} = State, Pids, true) ->
    {Pid, Ref} = spawn_monitor(shutdown_and_kill_after_timeout_fun(Pids)),
    State#state{tasks = Tasks#{Ref => Pid}}.

-spec shutdown_and_kill_after_timeout_fun([pid()]) -> fun(() -> term()).
shutdown_and_kill_after_timeout_fun(Pids) ->
    fun() ->
            [ exit(Pid, shutdown) || Pid <- Pids ],
            timer:sleep(?SHUTDOWN_TIMEOUT),
            [ exit(Pid, kill) || Pid <- Pids ]
    end.
