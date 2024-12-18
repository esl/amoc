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

-export([start_children/4, user_up/2, stop_children/3]).

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

-spec user_up(pid(), amoc_scenario:user_id()) -> any().
user_up(Sup, Id) ->
    erlang:send(Sup, {user_up, self(), Id}).

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
handle_cast({stop_children, Pids, ForceRemove}, State) ->
    NewState = do_stop_children(State, Pids, ForceRemove),
    {noreply, NewState};
handle_cast(_Info, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Request, state()) -> {noreply, state()} when
      Request :: {user_up, pid(), amoc_scenario:user_id()} |
                 {'EXIT', pid(), term()} |
                 {_, reference(), process, pid(), term()}.
handle_info({user_up, Pid, Id}, State) ->
    NewState = handle_up_user(State, Pid, Id),
    {noreply, NewState};
handle_info({user_down, _Ref, process, Pid, _Reason},
            #state{waiting = Waiting} = State) ->
    NewState = handle_down_user(State, Pid),
    {noreply, NewState#state{waiting = maps:remove(Pid, Waiting)}};
handle_info({task_down, Ref, process, _Pid, _Reason}, #state{tasks = Tasks} = State) ->
    {noreply, State#state{tasks = maps:remove(Ref, Tasks)}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> any().
terminate(_Reason, #state{waiting = Waiting, tasks = Tasks}) ->
    {TaskPid, TaskRef} = shutdown_and_kill_after_timeout(maps:keys(Waiting)),
    maps:foreach(fun flush_task/2, Tasks#{TaskRef => TaskPid}).

%% Helpers
-spec do_start_children(state(), amoc:scenario(), [amoc_scenario:user_id()], term()) -> state().
do_start_children(#state{waiting = Waiting} = State, Scenario, Ids, ScenarioState) ->
    Fun = fun(Id, Acc) ->
                  {Pid, Ref} = user_up(Scenario, Id, ScenarioState),
                  [{Pid, Ref} | Acc]
          end,
    NewWaiting = lists:foldl(Fun, [], Ids),
    State#state{waiting = maps:merge(Waiting, maps:from_list(NewWaiting))}.

-spec user_up(amoc:scenario(), amoc_scenario:user_id(), state()) -> {pid(), reference()}.
user_up(Scenario, Id, State) ->
    Args = [self(), Scenario, Id, State],
    Opts = [link, {monitor, [{tag, user_down}]}],
    proc_lib:spawn_opt(amoc_user, init, Args, Opts).

-spec handle_up_user(state(), pid(), amoc_scenario:user_id()) -> state().
handle_up_user(#state{index = SupNum, waiting = Waiting} = State, Pid, Id) ->
    amoc_users_sup:handle_up_user(SupNum, Pid, Id),
    State#state{waiting = maps:remove(Pid, Waiting)}.

-spec handle_down_user(state(), pid()) -> state().
handle_down_user(#state{index = SupNum} = State, Pid) ->
    amoc_users_sup:handle_down_user(SupNum, Pid),
    State.

-spec flush_task(reference(), pid()) -> ok.
flush_task(Ref, Pid) ->
    receive {task_down, Ref, process, Pid, _} -> ok after 0 -> ok end.

%% @doc Stop a list of users in parallel.
%% We don't want to ever block the supervisor on `timer:sleep/1' so we spawn that async.
%% However we don't want free processes roaming around, we want monitoring that can be traced.
-spec do_stop_children(state(), [pid()], boolean()) -> state().
do_stop_children(State, Pids, false) ->
    [ exit(Pid, shutdown) || Pid <- Pids ],
    State;
do_stop_children(#state{tasks = Tasks} = State, Pids, true) ->
    {Pid, Ref} = shutdown_and_kill_after_timeout(Pids),
    State#state{tasks = Tasks#{Ref => Pid}}.

-spec shutdown_and_kill_after_timeout_fun([pid()]) -> fun(() -> term()).
shutdown_and_kill_after_timeout_fun(Pids) ->
    fun() ->
            [ exit(Pid, shutdown) || Pid <- Pids ],
            timer:sleep(?SHUTDOWN_TIMEOUT),
            [ exit(Pid, kill) || Pid <- Pids ]
    end.

-spec shutdown_and_kill_after_timeout([pid()]) -> {pid(), reference()}.
shutdown_and_kill_after_timeout(Pids) ->
    Fun = shutdown_and_kill_after_timeout_fun(Pids),
    erlang:spawn_opt(Fun, [{monitor, [{tag, task_down}]}]).
