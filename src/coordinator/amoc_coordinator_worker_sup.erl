%% @private
%% @see amoc_coordinator
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc Supervisor for a pool of handlers for a new supervision tree
-module(amoc_coordinator_worker_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-spec start_link({amoc_coordinator:name(), amoc_coordinator:coordination_plan(), timeout()}) ->
    supervisor:startlink_ret().
start_link({Name, Plan, Timeout}) ->
    supervisor:start_link(?MODULE, {Name, Plan, Timeout}).

-spec init({amoc_coordinator:name(), amoc_coordinator:coordination_plan(), timeout()}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Name, Plan, Timeout}) ->
    %% Order matters, as we need to ensure that:
    %%  - all the items in the plan with NoOfUsers =:= all are executed last
    %%  - the timeout handler is executed first
    {All, NotAll} = lists:partition(fun partitioner/1, Plan),
    OrderedPlan = lists:reverse(All) ++ lists:reverse(NotAll),
    OrderedChilds = [ worker_spec(Item) || Item <- OrderedPlan ],
    TimeoutChild = timeout_child(Name, Timeout),
    Childs = [TimeoutChild | OrderedChilds],

    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, {SupFlags, Childs}}.

%% Helpers
timeout_child(Name, Timeout) ->
    #{id => {amoc_coordinator_timeout, Name, Timeout},
      start => {amoc_coordinator_timeout, start_link, [Name, Timeout]},
      restart => permanent,
      shutdown => timer:seconds(5),
      type => worker,
      modules => [amoc_coordinator_timeout]}.

worker_spec(Item) ->
    #{id => {amoc_coordinator_worker, Item},
      start => {amoc_coordinator_worker, start_link, [Item]},
      restart => permanent,
      shutdown => timer:seconds(5),
      type => worker,
      modules => [amoc_coordinator_worker]}.

partitioner({all, _}) ->
    true;
partitioner(_) ->
    false.
