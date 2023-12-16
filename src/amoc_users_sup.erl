%% @private
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc Supervisor-like gen_server with some tracking responsibilities over the users
%%
%% We want to keep consistent track of all users running globally by running special code upon a
%% children's death. Standard solutions don't cut it because:
%%  - A supervisor doesn't expose callbacks on user termination
%%  - Implementing code on the user process before it dies risks inconsistencies if it is killed
%%  More improvements that could be made would be to distribute the supervision tree like ranch did,
%%  see https://stressgrid.com/blog/100k_cps_with_elixir/
%% @end
-module(amoc_users_sup).

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
          id :: non_neg_integer(),
          tid :: ets:tid(),
          tasks = #{} :: #{reference() => pid()}
         }).
-type state() :: #state{}.

-define(SHUTDOWN_TIMEOUT, 2000). %% 2 seconds

%% @private
-spec start_link(non_neg_integer()) -> {ok, pid()}.
start_link(N) ->
    gen_server:start_link(?MODULE, N, []).

%% @private
-spec init(non_neg_integer()) -> {ok, term()}.
init(N) ->
    process_flag(trap_exit, true),
    Tid = ets:new(?MODULE,  [ordered_set, private]),
    {ok, #state{id = N, tid = Tid}}.

%% @private
-spec handle_call(any(), any(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%% @private
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({start_child, Scenario, Id, ScenarioState}, #state{tid = Tid} = State) ->
    case amoc_user:start_link(Scenario, Id, ScenarioState) of
        {ok, Pid} ->
            handle_up_user(Tid, Pid, Id),
            {noreply, State#state{}};
        _ ->
            {noreply, State}
    end;
handle_cast({stop_child, ForceRemove}, #state{tid = Tid} = State) ->
    Pids = case ets:match_object(Tid, '$1', 1) of
               {[{Pid, _Id}], _} -> [Pid];
               '$end_of_table' -> []
           end,
    NewState = maybe_track_task_to_stop_my_children(State, Pids, ForceRemove),
    {noreply, NewState};
handle_cast(terminate_all_children, State) ->
    NewState = do_terminate_all_my_children(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{tasks = Tasks} = State) ->
    {noreply, State#state{tasks = maps:remove(Ref, Tasks)}};
handle_info({'EXIT', Pid, _Reason}, #state{tid = Tid} = State) ->
    handle_down_user(Tid, Pid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> any().
terminate(_Reason, State) ->
    do_terminate_all_my_children(State).

%% Helpers

-spec handle_up_user(ets:tid(), pid(), amoc_scenario:user_id()) -> ok.
handle_up_user(Tid, Pid, Id) ->
    ets:insert(Tid, {Pid, Id}),
    amoc_users_sup_sup:incr_no_of_users().

-spec handle_down_user(ets:tid(), pid()) -> ok.
handle_down_user(Tid, Pid) ->
    ets:delete(Tid, Pid),
    amoc_users_sup_sup:decr_no_of_users().

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
    Fun = fun() ->
                  [ exit(Pid, shutdown) || Pid <- Pids ],
                  timer:sleep(?SHUTDOWN_TIMEOUT),
                  [ exit(Pid, kill) || Pid <- Pids ]
          end,
    {Pid, Ref} = spawn_monitor(Fun),
    State#state{tasks = Tasks#{Pid => Ref}}.

-spec do_terminate_all_my_children(state()) -> any().
do_terminate_all_my_children(#state{tid = Tid} = State) ->
    Match = ets:match_object(Tid, '$1', 200),
    do_terminate_all_my_children(State, Match).

%% ets:continuation/0 type is unfortunately not exported from the ets module.
-spec do_terminate_all_my_children(state(), {tuple(), term()} | '$end_of_table') -> state().
do_terminate_all_my_children(State, {Objects, Continuation}) ->
    Pids = [Pid || {Pid, _Id} <- Objects],
    NewState = maybe_track_task_to_stop_my_children(State, Pids, true),
    Match = ets:match_object(Continuation),
    do_terminate_all_my_children(NewState, Match);
do_terminate_all_my_children(State, '$end_of_table') ->
    State.
