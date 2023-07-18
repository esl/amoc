%%==============================================================================
%% @doc This module allows to synchronize the users and act on groups of them.
%%
%% The coordinator reacts to new users showing up in a system, according to the <i>Coordination Plan</i>.
%% The <i>Coordination Plan</i> consists of <i>Coordination Items</i>,
%% and each of them is defined as one of the following: `{NumberOfUsers, CoordinationActions}'.
%%
%% <ul>
%% <li> When the `NumberOfUsers' is set to `all', then only <i>Coordination Actions</i> with the arities `/1' and `/2' are handled. The <i>Coordination Items</i> with `all' are triggered by the `timeout' event type.</li>
%% <li> When the `NumberOfUsers' is set to a positive integer, all <i>Coordination Actions</i> with arities `/1', `/2' and `/3' are handled.</li>
%% </ul>
%%
%% The timeout timer is reset by calling the `add' function.
%% A new batch size is set in the `NumberOfUsers'.
%% Each user in the batch calls the `add' function registering to the coordinator and triggering the <i>Coordination Plan</i>.
%% If more than one of the <i>Coordination Items</i> matching the `NumberOfUsers' is triggered, each of them will be passed the respective number of users.
%% For example if the <i>Coordination Plan</i> is `[{2, Act1}, {3, Act2}]' then on the 6th user calling `add',
%% `Act1' will be called with 2 users passed and `Act2' will be called with 3 users passed.
%%
%% <i>Coordination Actions</i> may be one of the following:
%%
%% <ul>
%% <li> `fun(Event) -> any()' - this type of action does not care about particular users, but only about the number of them;</li>
%% <li> `fun(Event, ListOfUsersData) -> any()' - this type of action gets `ListOfUsersData' which is a list of `{Pid, Data}' tuples with `Pid's passed by users calling `amoc_coordinator:add/2' or `amoc_coordinator:add/3';</li>
%% <li> `fun(Event, User1, User2) -> any()' - this type of action gets `distinct pairs' from the batch of users `User1' and `User2' which are `{Pid, Data}' tuples with `Pid's passed by users calling `amoc_coordinator:add/2' or `amoc_coordinator:add/3';</li>
%% </ul>
%%
%% where an `Event' is a `{EventType, NumOfUsers}' tuple, in which `NumOfUsers' is the number of users passed to the event.
%%
%% The order of <i>Coordination Actions</i> execution is not guaranteed.
%% It’s guaranteed that all the <i>Coordination Actions</i> with `all' are executed after
%% all numbered <i>Coordination Actions</i> are done.
%%
%% `distinct pairs' - in the context, these are pairs from a given collection of users, where:
%% <ul>
%% <li> when `{A, B}' is in the `distinct pairs' then `{B, A}' is not;</li>
%% <li> `{A, A}' is not in the `distinct pairs';</li>
%% <li> all pairs are distinct;</li>
%% <li> Eg. for `[a]', the `distinct pairs' collection is `[{a, undefined}]';</li>
%% <li> Eg. for `[a, b]', the `distinct pairs' collection is `[{a, b}]';</li>
%% <li> Eg. for `[a, b, c]', the `distinct pairs' collection is `[{a, b}, {a, c}, {b, c}]'.</li>
%% </ul>
%%
%% <h2>Example:</h2>
%%
%% This scenario will demonstrate how do the `users' interact with `amoc_coordinator':
%%
%% ```
%% -module(example).
%%
%% -export([init/0]).
%% -export([start/2]).
%%
%% init() ->
%%     Plan = [
%%         {2, fun(Event) ->
%%                 io:fwrite("Two new users showed up: Event = ~p\n", [Event])
%%             end},
%%         {2, fun(Event, ListOfUsers) ->
%%                 io:fwrite("Two new users showed up: Event = ~p; ListOfUsers = ~p\n", [Event, ListOfUsers]),
%%                 [ Pid ! {hello, Data} || {Pid, Data} <- ListOfUsers]
%%             end},
%%         {2, fun(Event, User1, User2) ->
%%                 io:fwrite("Two new users showed up: Event = ~p; User1 = ~p; User2 = ~p\n", [Event, User1, User2])
%%             end},
%%         {3, fun(_Event) ->
%%                 io:fwrite("Three new users showed up\n", [])
%%             end},
%%         {all, fun(Event) ->
%%                 io:fwrite("All users have called amoc_coordinator:add in Event = ~p\n", [Event])
%%             end}
%%     ],
%%     Settings = [setting1, {setting2, something}],
%%     amoc_coordinator:start(?MODULE, Plan),
%%     {ok, Settings}.
%%
%% start(Id, _Settings) ->
%%     io:fwrite("User = ~p\n", [Id]),
%%     amoc_coordinator:add(?MODULE, Id),
%%     receive
%%         Msg ->
%%             io:fwrite("{Msg = ~p, Id = ~p\n", [Msg, Id])
%%     end,
%%     ok.
%% '''
%%
%%
%% To run it:
%%
%% ```
%% $ make rel
%% $ _build/default/rel/amoc/bin/amoc console
%%
%% 1> amoc:do(example, 5, []).
%% '''
%%
%% Filtered, formated and explained output:
%%
%% ```
%% User = 1  % First user is started
%%
%% ok  % result of calling amoc:do/3
%%
%% User = 2 % First user is started
%%
%% % We have 2 users added to amoc_coordinator so all of actions {2, _} are triggered:
%% Two new users showed up: Event = {coordinate,2}; User1 = {<0.1142.0>,2}; User2 = {<0.1140.0>,1}
%% % This action triggers sending {hello,Id} to the users 1 and 2
%% Two new users showed up: Event = {coordinate,2}; ListOfUsers = [{<0.1142.0>,2},{<0.1140.0>,1}]
%% Two new users showed up: Event = {coordinate,2}
%%
%% % Users 1 and 2 received messages and print them:
%% {Msg = {hello,2}, Id = 2
%% {Msg = {hello,1}, Id = 1
%%
%% User = 3
%% % We have 3 users added to amoc_coordinator so all of the {3, _} actions are triggered:
%% Three new users showed up
%%
%% User = 4
%% % We have 4 and 4 rem 2 == 0 therefore users added to amoc_coordinator so all of the {3, _} actions are triggered:
%% Two new users showed up: Event = {coordinate,2}; User1 = {<0.1144.0>,4}; User2 = {<0.1143.0>,3}
%% Two new users showed up: Event = {coordinate,2}; ListOfUsers = [{<0.1144.0>,4},{<0.1143.0>,3}]
%% Two new users showed up: Event = {coordinate,2}
%%
%% {Msg = {hello,4}, Id = 4
%% {Msg = {hello,3}, Id = 3
%% User = 5
%%
%% % You need to wait for a while, and ...
%% % Timeout has been reached, which triggers all of the Coordination Actions with the remaining number of users.
%% Three new users showed up
%% Two new users showed up: Event = {timeout,1}; User1 = {<0.1139.0>,5}; User2 = undefined
%% Two new users showed up: Event = {timeout,1}; ListOfUsers = [{<0.1139.0>,5}]
%% {Msg = {hello,5}, Id = 5
%% Two new users showed up: Event = {timeout,1}
%% All users have called amoc_coordinator:add in Event = {timeout,5}
%% '''
%%
%% @copyright 2023 Erlang Solutions Ltd.
%%==============================================================================
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

-define(DEFAULT_TIMEOUT, 30). %% 30 seconds

-define(IS_POS_INT(Integer), (is_integer(Integer) andalso Integer > 0)).
-define(IS_N_OF_USERS(N), (?IS_POS_INT(N) orelse N =:= all)).
-define(IS_TIMEOUT(Timeout), (?IS_POS_INT(Timeout) orelse Timeout =:= infinity)).

-type name() :: atom().
-type state() :: {worker, name(), pid()} | {timeout, name(), pid()}.

-type coordination_data() :: {pid(), Data :: any()}.

-type maybe_coordination_data() :: coordination_data() | undefined.

-type coordination_event_type() :: coordinate | timeout | stop | reset.

-type coordination_event() :: {coordination_event_type(), non_neg_integer()}.

-type coordination_action() ::
    fun((coordination_event(), [coordination_data()]) -> any()) |
    fun((coordination_event(), maybe_coordination_data(), maybe_coordination_data()) -> any()) |
    fun((coordination_event()) -> any()).

-type coordination_actions() :: [coordination_action()] | coordination_action().

-type coordination_item() :: {NoOfUsers :: pos_integer() | all,
                              coordination_actions()}.

-type normalized_coordination_item() :: {NoOfUsers :: pos_integer() | all,
                                         [coordination_action()]}.

-type coordination_plan() :: [coordination_item()] | coordination_item().

%% timeout in seconds
-type coordination_timeout_in_sec() :: pos_integer() | infinity.

-export_type([coordination_event_type/0,
              coordination_event/0,
              coordination_action/0,
              coordination_data/0,
              coordination_plan/0,
              normalized_coordination_item/0]).

%%%===================================================================
%%% Api
%%%===================================================================

%% @see start/3
-spec start(name(), coordination_plan()) -> ok | error.
start(Name, CoordinationPlan) ->
    start(Name, CoordinationPlan, ?DEFAULT_TIMEOUT).

%% @doc Starts a coordinator. Usually called in the init callback of an amoc scenario.
-spec start(name(), coordination_plan(), coordination_timeout_in_sec()) -> ok | error.
start(Name, CoordinationPlan, Timeout) when ?IS_TIMEOUT(Timeout) ->
    Plan = normalize_coordination_plan(CoordinationPlan),
    case gen_event:start({local, Name}) of
        {ok, _} ->
            %% according to gen_event documentation:
            %%
            %%    When the event is received, the event manager calls
            %%    handle_event(Event, State) for each installed event
            %%    handler, in the same order as they were added.
            %%
            %% in reality the order is reversed, the last added handler
            %% is executed at first. so to ensure that all the items in
            %% the plan with NoOfUsers =:= all are executed in the very
            %% end, we need to add them first.
            AllItemsHandlers = lists:reverse([Item || {all, _} = Item <- Plan]),
            [gen_event:add_handler(Name, ?MODULE, {Name, Item}) || Item <- AllItemsHandlers],
            [gen_event:add_handler(Name, ?MODULE, {Name, Item}) || {N, _} = Item <- Plan, is_integer(N)],
            gen_event:add_handler(Name, ?MODULE, {timeout, Name, Timeout}),
            ok;
        {error, _} -> error
    end.

%% @doc Stops a coordinator.
-spec stop(name()) -> ok.
stop(Name) ->
    gen_event:stop(Name).

%% @see add/3
-spec add(name(), any()) -> ok.
add(Name, Data) ->
    add(Name, self(), Data).

%% @doc Adds the current process data. Usually called in the `start/2' callback of an amoc scenario.
-spec add(name(), pid(), any()) -> ok.
add(Name, Pid, Data) ->
    gen_event:notify(Name, {coordinate, {Pid, Data}}).

%% @doc Resets a coordinator, that is, calls all coordination actions with `reset' as the coordination data.
-spec reset(name()) -> ok.
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
-spec init({timeout, name(), coordination_timeout_in_sec()} | normalized_coordination_item()) ->
    {ok, state()}.
init({timeout, Name, Timeout}) ->
    Pid = spawn(fun() ->
                    case Timeout of
                        infinity ->
                            timeout_fn(Name, infinity, infinity);
                        Int when is_integer(Int), Int > 0 ->
                            timeout_fn(Name, timer:seconds(Timeout), infinity)
                    end
                end),
    {ok, {timeout, Name, Pid}};
init({Name, CoordinationItem}) ->
    {ok, Pid} = amoc_coordinator_worker:start_link(CoordinationItem),
    {ok, {worker, Name, Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event :: term(), state()) -> {ok, state()}.
handle_event(Event, {timeout, Name, Pid}) ->
    erlang:send(Pid, Event),
    {ok, {timeout, Name, Pid}};
handle_event(Event, {worker, Name, Pid}) ->
    telemetry:execute([amoc, coordinator, event], #{count => 1}, #{name => Name, type => Event}),
    case Event of
        coordinator_timeout -> %% synchronous notification
            amoc_coordinator_worker:timeout(Pid);
        reset_coordinator -> %% synchronous notification
            amoc_coordinator_worker:reset(Pid);
        {coordinate, Data} -> %% asnyc notification
            amoc_coordinator_worker:add(Pid, Data)
    end,
    {ok, {worker, Name, Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), state()) -> {ok, {error, not_implemented}, state()}.
handle_call(_Request, State) ->
    {ok, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(_, {timeout, _Name, Pid}) ->
    erlang:send(Pid, terminate), ok;
terminate(_, {worker, _Name, Pid}) ->
    %% synchronous notification
    amoc_coordinator_worker:stop(Pid), ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec normalize_coordination_plan(coordination_plan()) -> [normalized_coordination_item()].
normalize_coordination_plan(CoordinationPlan) when is_tuple(CoordinationPlan) ->
    normalize_coordination_plan([CoordinationPlan]);
normalize_coordination_plan(CoordinationPlan) ->
    [normalize_coordination_item(I) || I <- CoordinationPlan].

normalize_coordination_item({NoOfUsers, Action}) when is_function(Action) ->
    normalize_coordination_item({NoOfUsers, [Action]});
normalize_coordination_item({NoOfUsers, Actions}) when ?IS_N_OF_USERS(NoOfUsers),
                                                       is_list(Actions) ->
    [assert_action(NoOfUsers, A) || A <- Actions],
    {NoOfUsers, Actions}.

assert_action(all, Action) when is_function(Action, 1);
                                is_function(Action, 2) ->
    ok;
assert_action(N, Action) when is_integer(N),
                              (is_function(Action, 1) orelse
                               is_function(Action, 2) orelse
                               is_function(Action, 3)) ->
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
