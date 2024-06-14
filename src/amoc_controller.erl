%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Main controller of a node, responsible for the scenario and the users
%%
%% Note that this module should be rarely used, APIs are fully exposed by `amoc' and `amoc_dist'
%% for local or distributed environments respectively.
%% @end
-module(amoc_controller).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-required_variable(#{name => interarrival, default_value => 50,
                     verification => {?MODULE, non_neg_integer, 1},
                     description => "a delay between creating the processes for two "
                                    "consecutive users (ms, def: 50ms)",
                     update => {?MODULE, maybe_update_interarrival_timer, 2}}).

-record(state, {scenario :: amoc:scenario() | undefined,
                last_user_id = 0 :: last_user_id(),
                status = idle :: idle | running | terminating | finished |
                                 {error, any()} | disabled,
                scenario_state :: any(), %% state returned from Scenario:init/0
                create_users = [] :: [amoc_scenario:user_id()],
                tref :: timer:tref() | undefined}).

-type state() :: #state{}.
%% Internal state of the node's controller
-type handle_call_res() :: ok | {ok, term()} | {error, term()}.
-type running_status() :: #{scenario := amoc:scenario(),
                            currently_running_users := user_count(),
                            highest_user_id := last_user_id()}.
%% Details about the scenario currently running
-type amoc_status() :: idle |
                       {running, running_status()} |
                       {terminating, amoc:scenario()} |
                       {finished, amoc:scenario()} |
                       {error, any()} |
                       disabled.
%% Status of the node, note that amoc_controller is disabled for the master node
-type user_count() :: non_neg_integer().
%% Number of users currently running in the node
-type last_user_id() :: non_neg_integer().
%% Highest user id registered in the node
-type interarrival() :: non_neg_integer().
%% Time to wait in between spawning new users

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
-export([maybe_update_interarrival_timer/2, non_neg_integer/1]).

-export([zero_users_running/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @private
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_scenario(amoc:scenario(), amoc_config:settings()) ->
    ok | {error, term()}.
start_scenario(Scenario, Settings) ->
    case amoc_code_server:does_scenario_exist(Scenario) of
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
add_users(StartId, EndId) ->
    %% adding the exact range of the users
    gen_server:call(?SERVER, {add, StartId, EndId}).

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

%% @private
-spec non_neg_integer(any()) -> boolean().
non_neg_integer(Interarrival) ->
    is_integer(Interarrival) andalso Interarrival >= 0.

%% @private
-spec maybe_update_interarrival_timer(interarrival, term()) -> ok.
maybe_update_interarrival_timer(interarrival, _) ->
    gen_server:cast(?SERVER, maybe_update_interarrival_timer).

%% @private
-spec zero_users_running() -> ok.
zero_users_running() ->
    gen_server:cast(?SERVER, zero_users_running).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
-spec init([]) -> {ok, state()}.
init([]) ->
    start_tables(),
    {ok, #state{}}.

%% @private
%% We set the priority to high after starting the scenario,
%% and then reset priority to normal after terminating it.
%% The most important part is precise timing for users spawning/removal,
%% so priority is higher in between init and terminate.
-spec handle_call(any(), any(), state()) -> {reply, handle_call_res(), state()}.
handle_call({start_scenario, Scenario, Settings}, _From, State) ->
    {RetValue, NewState} = handle_start_scenario(Scenario, Settings, State),
    process_flag(priority, high),
    {reply, RetValue, NewState};
handle_call(stop_scenario, _From, State) ->
    {RetValue, NewState} = handle_stop_scenario(State),
    process_flag(priority, normal),
    {reply, RetValue, NewState};
handle_call({update_settings, Settings}, _From, State) ->
    RetValue = handle_update_settings(Settings, State),
    {reply, RetValue, State};
handle_call({add, StartId, EndId}, _From, State) ->
    {RetValue, NewState} = handle_add(StartId, EndId, State),
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

%% @private
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(maybe_update_interarrival_timer, State) ->
    {noreply, maybe_update_interarrival_timer(State)};
handle_cast(zero_users_running, State) ->
    NewSate = handle_zero_users_running(State),
    {noreply, NewSate};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(start_user, State) ->
    NewSate = handle_start_user(State),
    {noreply, NewSate};
handle_info(start_all_users, State) ->
    NewSate = handle_start_all_users(State),
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
            NewState = State#state{last_user_id   = 0,
                                   scenario       = Scenario,
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
    case amoc_users_sup:count_no_of_users() of
        0 ->
            terminate_scenario(State),
            {ok, State#state{status = finished}};
        _ ->
            amoc_users_sup:terminate_all_children(),
            {ok, State#state{status = terminating}}
    end;
handle_stop_scenario(#state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_update_settings(amoc_config:settings(), state()) -> handle_call_res().
handle_update_settings(Settings, #state{status = running}) ->
    case amoc_config_scenario:update_settings(Settings) of
        ok -> ok;
        {error, Type, Reason} -> {error, {Type, Reason}}
    end;
handle_update_settings(_Settings, #state{status = Status}) ->
    {error, {invalid_status, Status}}.

-spec handle_add(amoc_scenario:user_id(), amoc_scenario:user_id(), state()) ->
    {handle_call_res(), state()}.
handle_add(StartId, EndId, #state{last_user_id = LastId,
                                  create_users = ScheduledUsers,
                                  status       = running,
                                  scenario     = Scenario,
                                  tref         = TRef} = State) when StartId =< EndId,
                                                                     LastId < StartId ->
    amoc_telemetry:execute([controller, users], #{count => EndId - StartId + 1},
                           #{scenario => Scenario, type => add}),
    NewUsers = lists:seq(StartId, EndId),
    NewScheduledUsers = lists:append(ScheduledUsers, NewUsers),
    NewTRef = maybe_start_timer(TRef),
    {ok, State#state{create_users = NewScheduledUsers, tref = NewTRef, last_user_id = EndId}};
handle_add(_StartId, _EndId, #state{status = running} = State) ->
    {{error, invalid_range}, State};
handle_add(_StartId, _EndId, #state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_remove(user_count(), boolean(), state()) -> handle_call_res().
handle_remove(Count, ForceRemove, #state{status = running, scenario = Scenario}) ->
    CountRemove = amoc_users_sup:stop_children(Count, ForceRemove),
    amoc_telemetry:execute([controller, users], #{count => CountRemove},
                           #{scenario => Scenario, type => remove}),
    {ok, CountRemove};
handle_remove(_Count, _ForceRemove, #state{status = Status}) ->
    {error, {invalid_status, Status}}.

-spec handle_status(state()) -> amoc_status().
handle_status(#state{status = running, scenario = Scenario,
                     last_user_id = LastId}) ->
    N = amoc_users_sup:count_no_of_users(),
    {running, #{scenario => Scenario, currently_running_users => N, highest_user_id => LastId}};
handle_status(#state{status = terminating, scenario = Scenario}) ->
    {terminating, Scenario};
handle_status(#state{status = finished, scenario = Scenario}) ->
    {finished, Scenario};
handle_status(#state{status = Status}) ->
    Status. %% idle, disabled or {error, Reason}.

-spec handle_disable(state()) -> {handle_call_res(), state()}.
handle_disable(#state{status = idle} = State) ->
    {ok, State#state{status = disabled}};
handle_disable(#state{status = Status} = State) ->
    {{error, {invalid_status, Status}}, State}.

-spec handle_start_user(state()) -> state().
handle_start_user(#state{create_users   = [UserId | T],
                         scenario       = Scenario,
                         scenario_state = ScenarioState} = State) ->
    amoc_users_sup:start_child(Scenario, UserId, ScenarioState),
    State#state{create_users = T};
handle_start_user(#state{create_users = [], tref = TRef} = State) ->
    State#state{tref = maybe_stop_timer(TRef)}.

-spec handle_start_all_users(state()) -> state().
handle_start_all_users(#state{create_users   = AllUsers,
                              scenario       = Scenario,
                              scenario_state = ScenarioState,
                              tref = TRef} = State) ->
    amoc_users_sup:start_children(Scenario, AllUsers, ScenarioState),
    State#state{create_users = [], tref = maybe_stop_timer(TRef)}.

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------
-spec start_tables() -> ok.
start_tables() -> %% ETS creation
    amoc_config_utils:create_amoc_config_ets(),
    ok.

-spec init_scenario(amoc:scenario(), amoc_config:settings()) ->
    {ok | error, any()}.
init_scenario(Scenario, Settings) ->
    case amoc_config_scenario:parse_scenario_settings(Scenario, Settings) of
        ok -> amoc_scenario:init(Scenario);
        {error, Type, Reason} -> {error, {Type, Reason}}
    end.

-spec terminate_scenario(state()) -> ok | {ok, any()} | {error, any()}.
terminate_scenario(#state{scenario = Scenario, scenario_state = ScenarioState}) ->
    amoc_scenario:terminate(Scenario, ScenarioState).

-spec handle_zero_users_running(state()) -> state().
handle_zero_users_running(#state{status = terminating} = State) ->
    terminate_scenario(State),
    State#state{status = finished};
handle_zero_users_running(State) ->
    State.

-spec maybe_stop_timer(timer:tref() | undefined) -> undefined.
maybe_stop_timer(undefined) ->
    undefined;
maybe_stop_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    undefined.

-spec get_interarrival() -> interarrival().
get_interarrival() ->
    amoc_config:get(interarrival).

-spec maybe_update_interarrival_timer(state()) -> state().
maybe_update_interarrival_timer(#state{tref = undefined} = State) ->
    State;
maybe_update_interarrival_timer(#state{tref = TRef} = State) ->
    {ok, cancel} = timer:cancel(TRef),
    Value = get_interarrival(),
    NewTRef = do_interarrival(Value),
    State#state{tref = NewTRef}.

-spec maybe_start_timer(timer:tref() | undefined) -> timer:tref().
maybe_start_timer(undefined) ->
    Value = get_interarrival(),
    do_interarrival(Value);
maybe_start_timer(TRef) -> TRef.

do_interarrival(0) ->
    self() ! start_all_users,
    undefined;
do_interarrival(Value) ->
    {ok, NewTRef} = timer:send_interval(Value, start_user),
    NewTRef.
