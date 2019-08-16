-module(coordinator).

-behaviour(gen_server).

-include_lib("escalus/include/escalus.hrl").


%% API
-export([start_link/2]).

-export([register_user/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-define(SERVER, ?MODULE).

-define(COORDINATOR_TIMEOUT, 100000).

-type client() :: #client{}.
-type plan() :: [{EveryNClients :: pos_integer() | all,
    DoFun :: fun(([{Pid :: pid(), Client :: client()}]) -> any())}].

-type state() :: [{EveryNClients :: pos_integer() | all,
    [User :: {pid(), client()}],
    DoFun :: fun(([{Pid :: pid(), Client :: client()}]) -> any())}].

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(Plan :: plan(), Settings :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Plan, Settings) ->
    lager:debug("Start_link, Plan: ~p, ~p", [Plan, Settings]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Plan, Settings], []).

-spec(register_user(Client :: client()) -> ok).
register_user(Client) ->
    lager:debug("Register_user ~p, client: ~p", [self(), Client]),
    gen_server:cast(?SERVER, {new_user, self(), Client}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Plan, Settings]) ->
    config:store_scenario_settings(Settings),
    config:dump_settings(),
    PlanWithClientLists = [{N, [], Fun} || {N, Fun} <- Plan],
    lager:debug("Plan with Clients: ~p", [PlanWithClientLists]),
    {ok, PlanWithClientLists}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec(handle_cast(Request :: {new_user, pid(), client()}, State :: state()) ->
    {noreply, NewState :: state(), timeout()} |
    {stop, Reason :: term(), NewState :: state()}).
handle_cast({new_user, Pid, Client}, State) ->
%%    lager:debug("Handle cast new_user ~p, client: ~p", [Pid, Client]),

    NewUser = {Pid, Client},
    ToExecute = [{N, [NewUser | Users], Fun} || {N, Users, Fun} <- State, is_integer(N), N == length(Users) + 1],
    NewState = [case length(Users) + 1 of
                    N -> {N, [], Fun};
                    _ -> {N, [NewUser | Users], Fun}
                end || {N, Users, Fun} <- State],
    execute(ToExecute),
    {noreply, NewState, ?COORDINATOR_TIMEOUT}.

-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State) ->
    execute(State),
    lager:error("Waited too long for the new user!"),
    NewState = [{N, [], Fun} || {N, _User, Fun} <- State],
    {noreply, NewState, ?COORDINATOR_TIMEOUT}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term()).
terminate(_Reason, _State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

execute(State) ->
    [Fun(Users) || {_N, Users, Fun} <- State].