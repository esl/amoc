-module(amoc_coordinator).

-behaviour(gen_event).

%% API
-export([start/3, start/2,
         add/2, add/3,
         stop/1, reset/1]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         terminate/2]).

-define(DEFAULT_TIMEOUT, 30000). %% 30 seconds

-define(is_pos_int(Integer), (is_integer(Integer) andalso Integer > 0)).
-define(is_timeout(Timeout), (?is_pos_int(Timeout) orelse Timeout =:= infinity)).

-type state() :: {worker, pid()} | {timeout, pid()}.

-type coordination_data() :: {pid(), Data :: any()}.

-type maybe_coordination_data() :: coordination_data() | undefined.

-type coordination_event() :: coordinate | timeout | stop | reset.

-type coordination_timeout() :: pos_integer() | infinity.

-type coordination_action() ::
    fun((coordination_event(), [coordination_data()]) -> any()) |
    fun((coordination_event(), maybe_coordination_data(), maybe_coordination_data()) -> any()) |
    fun((coordination_event()) -> any()).

-type coordination_actions() :: [coordination_action()] | coordination_action().

-type coordination_item_with_timeout() :: {NoOfUsers :: pos_integer(),
                                           coordination_actions(),
                                           coordination_timeout()}.

-type coordination_item_without_timeout() :: {NoOfUsers :: pos_integer(),
                                              coordination_actions()}.

-type coordination_item() :: coordination_item_without_timeout() |
                             coordination_item_with_timeout().

-type coordination_plan() :: [coordination_item()] | coordination_item().

-export_type([coordination_plan/0]).

%%%===================================================================
%%% Api
%%%===================================================================
-spec start(atom(), coordination_plan()) -> ok | error.
start(Name, CoordinationPlan) ->
    start(Name, CoordinationPlan, ?DEFAULT_TIMEOUT).

-spec start(atom(), coordination_plan(), coordination_timeout()) -> ok | error.
start(Name, CoordinationPlan, Timeout) ->
    case gen_event:start({local, Name}) of
        {ok, _} ->
            Plan = normalize_coordination_plan(CoordinationPlan),
            gen_event:add_handler(Name, ?MODULE, {timeout, Name, Timeout}),
            [gen_event:add_handler(Name, ?MODULE, I) || I <- Plan],
            ok;
        {error, _} -> error
    end.

-spec stop(atom()) -> ok.
stop(Name) ->
    gen_event:stop(Name).

-spec add(atom(), any()) -> ok.
add(Name, Data) ->
    add(Name, self(), Data).

-spec add(atom(), pid(), any()) -> ok.
add(Name, Pid, Data) ->
    gen_event:notify(Name, {coordinate, {Pid, Data}}).

-spec reset(atom()) -> ok.
reset(Name) ->
    gen_event:notify(Name, reset_coordinator).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init({timeout, atom(), coordination_timeout()} | coordination_item_with_timeout()) -> {ok, state()}).
init({timeout, Name, Timeout}) ->
    Pid = spawn(fun() -> timeout_fn(Name, Timeout, infinity) end),
    {ok, {timeout, Pid}};
init(CoordinationItem) ->
    {ok, Pid} = amoc_coordinator_worker:start(CoordinationItem),
    {ok, {worker, Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), state()) -> {ok, state()}).
handle_event(Event, {timeout, Pid}) ->
    erlang:send(Pid, Event),
    {ok, {timeout, Pid}};
handle_event(Event, {worker, Pid}) ->
    case Event of
        coordinator_timeout ->
            amoc_coordinator_worker:timeout(Pid);
        reset_coordinator ->
            amoc_coordinator_worker:reset(Pid);
        {coordinate, Data} ->
            amoc_coordinator_worker:add(Pid, Data)
    end,
    {ok, {worker, Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), state()) -> {ok, {error, not_implemented}, state()}).
handle_call(_Request, State) ->
    {ok, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(any(), state()) -> ok).
terminate(_, {timeout, Pid}) ->
    erlang:send(Pid, terminate), ok;
terminate(_, {worker, Pid}) ->
    amoc_coordinator_worker:stop(Pid), ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec normalize_coordination_plan(coordination_plan()) -> [coordination_item_with_timeout()].
normalize_coordination_plan(CoordinationPlan) when is_tuple(CoordinationPlan) ->
    normalize_coordination_plan([CoordinationPlan]);
normalize_coordination_plan(CoordinationPlan) ->
    [normalize_coordination_item(I) || I <- CoordinationPlan].

normalize_coordination_item({NoOfUsers, Actions}) ->
    normalize_coordination_item({NoOfUsers, Actions, infinity});
normalize_coordination_item({NoOfUsers, Action, Timeout}) when is_function(Action) ->
    normalize_coordination_item({NoOfUsers, [Action], Timeout});
normalize_coordination_item({NoOfUsers, Actions, Timeout}) when ?is_pos_int(NoOfUsers),
                                                                ?is_timeout(Timeout),
                                                                is_list(Actions) ->
    [assert_action(A) || A <- Actions],
    {NoOfUsers, Actions, Timeout}.

assert_action(Action) when is_function(Action, 1);
                           is_function(Action, 2);
                           is_function(Action, 3) ->
    ok.

timeout_fn(Name, CoordinationTimeout, Timeout) ->
    receive
        terminate -> ok;
        {coordinate, _} ->
            timeout_fn(Name, CoordinationTimeout, CoordinationTimeout);
        _ -> %% coordinator_timeout or reset_coordinator
            timeout_fn(Name, CoordinationTimeout, infinity)
    after Timeout ->
        gen_event:notify(Name, coordinator_timeout),
        timeout_fn(Name, CoordinationTimeout, infinity)
    end.