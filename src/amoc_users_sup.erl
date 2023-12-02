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
-define(USERS_TABLE, amoc_users).

%% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API for amoc_controller, always async to not block the controller
-export([start_child/3, stop_child/2,
         count_children/0, stop_children/2,
         terminate_all_children/0]).

-record(state, {
          no_of_users = 0 :: non_neg_integer()
         }).
-type state() :: #state{}.

-define(SHUTDOWN_TIMEOUT, 2000). %% 2 seconds

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec count_children() -> non_neg_integer().
count_children() ->
    ets:info(?USERS_TABLE, size).

-spec terminate_all_children() -> any().
terminate_all_children() ->
    gen_server:cast(?MODULE, terminate_all_children).

-spec start_child(amoc:scenario(), amoc_scenario:user_id(), any()) -> ok.
start_child(Scenario, Id, ScenarioState) ->
    gen_server:cast(?MODULE, {start_child, Scenario, Id, ScenarioState}).

-spec stop_children(non_neg_integer(), boolean()) -> non_neg_integer().
stop_children(Count, ForceRemove) ->
    Size = ets:info(?USERS_TABLE, size),
    CountRemove = min(Count, Size),
    gen_server:cast(?MODULE, {stop_children, CountRemove, ForceRemove}),
    CountRemove.

-spec stop_child(pid(), boolean()) -> ok | {error, any()}.
stop_child(Pid, true) ->
    Node = node(Pid),
    gen_server:call({?MODULE, Node}, {stop_child, Pid});
stop_child(Pid, false) when is_pid(Pid) ->
    exit(Pid, shutdown), %% do it in the same way as the supervisor
    ok.

%% ------------------------------------------------------------------
%% gen_server code
%% ------------------------------------------------------------------

%% @private
-spec init(term()) -> {ok, term()}.
init([]) ->
    Opts = [named_table, ordered_set, protected, {read_concurrency, true}],
    ?USERS_TABLE = ets:new(?USERS_TABLE, Opts),
    process_flag(trap_exit, true),
    process_flag(priority, high),
    {ok, #state{no_of_users = 0}}.

%% @private
-spec handle_call(any(), any(), state()) -> {reply, term(), state()}.
handle_call({stop_child, Pid}, _From, State) ->
    {Return, NewState} = handle_stop_user(Pid, State),
    {reply, Return, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%% @private
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({start_child, Scenario, Id, ScenarioState}, #state{no_of_users = N} = State) ->
    case amoc_user:start_link(Scenario, Id, ScenarioState) of
        {ok, Pid} ->
            ets:insert(?USERS_TABLE, {Pid, Id}),
            {noreply, State#state{no_of_users = N + 1}};
        _ ->
            {noreply, State}
    end;
handle_cast({stop_children, CountRemove, ForceRemove}, State) ->
    Pids = case ets:match_object(?USERS_TABLE, '$1', CountRemove) of
               {Objects, _} -> [Pid || {Pid, _Id} <- Objects];
               '$end_of_table' -> []
           end,
    do_stop_children(Pids, ForceRemove),
    {noreply, State};
handle_cast(terminate_all_children, State) ->
    do_terminate_all_children(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'EXIT', Pid, _Reason}, State) ->
    {_, NewState} = handle_stop_user(Pid, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    do_terminate_all_children().

%% @doc Stop users, possibly in parallel
%%
%% Stopping users one by one using supervisor:terminate_child/2 is
%% not an option because terminate_child requests are queued and
%% processed by the supervisor sequentially, and if the user process ignores
%% the `exit(Child, shutdown)' signal, that causes a `?SHUTDOWN_TIMEOUT' delay
%% before it's killed by `exit(Child, kill)'. So an attempt to remove N
%% users may take take `N * ?SHUTDOWN_TIMEOUT' milliseconds, which is
%% not acceptable. So let's do the same thing as the supervisor but in
%% parallel, so it won't result in a huge delay.
-spec do_stop_children([pid()], boolean()) -> ok.
do_stop_children(Pids, false) ->
    [stop_child(Pid, false) || Pid <- Pids],
    ok;
do_stop_children(Pids, true) ->
    spawn(
        fun() ->
            [exit(Pid, shutdown) || Pid <- Pids],
            timer:sleep(?SHUTDOWN_TIMEOUT),
            [exit(Pid, kill) || Pid <- Pids]
        end),
    ok.

handle_stop_user(Pid, State) ->
    case ets:member(?USERS_TABLE, Pid) of
        true ->
            ets:delete(?USERS_TABLE, Pid),
            {ok, dec_no_of_users(State)};
        false ->
            {{error, not_found}, State}
    end.

dec_no_of_users(#state{no_of_users = 1} = State) ->
    amoc_controller:zero_users_running(),
    State#state{no_of_users = 0};
dec_no_of_users(#state{no_of_users = N} = State) ->
    State#state{no_of_users = N - 1}.

-spec do_terminate_all_children() -> any().
do_terminate_all_children() ->
    Match = ets:match_object(?USERS_TABLE, '$1', 200),
    do_terminate_all_children(Match).

%% ets:continuation/0 type is unfortunately not exported from the ets module.
-spec do_terminate_all_children({tuple(), term()} | '$end_of_table') -> ok.
do_terminate_all_children({Objects, Continuation}) ->
    Pids = [Pid || {Pid, _Id} <- Objects],
    do_stop_children(Pids, true),
    Match = ets:match_object(Continuation),
    do_terminate_all_children(Match);
do_terminate_all_children('$end_of_table') -> ok.
