%% @private
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Top supervisor of the pooled users supervisor.
%%
%% It spawns a pool of workers as big as online schedulers. When starting a new user, as the user is
%% identified by ID, a worker will be chosen for this user based on its ID
%% (see `get_sup_from_user_id/1').
%%
%% The currently running number of users is stored in an atomic that all workers update and the
%% controller can read.
-module(amoc_users_sup).

-behaviour(supervisor).

%% Supervisor
-export([start_link/0, init/1]).

%% API
-export([handle_up_user/3, handle_down_user/2, count_no_of_users/0]).
-export([start_children/4, stop_child/2, stop_children/2, terminate_all_children/0]).
-export([distribute/2, get_all_children/0]).

-type count() :: non_neg_integer().
-type assignment() :: [{pid(), count()}].

-record(storage, {
          %% an array of atomics whose index works as follows:
          %% * index=1 - overall number of Users
          %% * index>1 - number of users supervised by worker
          user_count :: atomics:atomics_ref(),
          sups :: tuple(),
          sups_indexed :: [{pid(), pos_integer()}],
          sups_count :: pos_integer()
         }).

-define(TABLE, amoc_users_sup_table).

%% Supervisor

%% @private
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    Ret = supervisor:start_link({local, ?MODULE}, ?MODULE, no_args),
    UserSups = supervisor:which_children(?MODULE),
    IndexedSupsUnsorted = [ {Pid, N} || {{amoc_users_worker_sup, N}, Pid, _, _} <- UserSups ],
    IndexedSups = lists:keysort(2, IndexedSupsUnsorted),
    UserSupPidsTuple = list_to_tuple([ Pid || {Pid, _} <- IndexedSups ]),
    SupCount = tuple_size(UserSupPidsTuple),
    Atomics = atomics:new(1 + SupCount, [{signed, false}]),
    Storage = #storage{user_count = Atomics, sups = UserSupPidsTuple,
                       sups_indexed = IndexedSups, sups_count = SupCount},
    persistent_term:put(?MODULE, Storage),
    Ret.

%% @private
-spec init(no_args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(no_args) ->
    EtsOpts = [ordered_set, public, named_table,
               {read_concurrency, true}, {write_concurrency, auto}],
    _Table = ets:new(?TABLE, EtsOpts),
    Specs = [
             #{
               id => {amoc_users_worker_sup, N},
               start => {amoc_users_worker_sup, start_link, [N]},
               restart => permanent,
               shutdown => infinity,
               type => worker,
               modules => [amoc_users_worker_sup]
              }
             || N <- indexes() ],
    Strategy = #{strategy => one_for_one, intensity => 0},
    {ok, {Strategy, Specs}}.

%% We start from 2 to simplify user_count atomics management.
indexes() ->
    lists:seq(2, erlang:system_info(schedulers_online) + 1).

%% API
-spec count_no_of_users() -> count().
count_no_of_users() ->
    #storage{user_count = Atomics} = persistent_term:get(?MODULE),
    atomics:get(Atomics, 1).

-spec handle_up_user(non_neg_integer(), pid(), amoc_scenario:user_id()) -> any().
handle_up_user(SupNum, Pid, Id) when SupNum > 1 ->
    ets:insert(?TABLE, {Pid, Id}),
    #storage{user_count = Atomics} = persistent_term:get(?MODULE),
    atomics:add(Atomics, SupNum, 1),
    atomics:add(Atomics, 1, 1).

-spec handle_down_user(non_neg_integer(), pid()) -> ok.
handle_down_user(SupNum, Pid) when SupNum > 1 ->
    ets:delete(?TABLE, Pid),
    #storage{user_count = Atomics} = persistent_term:get(?MODULE),
    atomics:sub(Atomics, SupNum, 1),
    case atomics:sub_get(Atomics, 1, 1) of
        0 ->
            amoc_controller:zero_users_running();
        _ ->
            ok
    end.

-spec stop_child(pid(), boolean()) -> ok.
stop_child(Pid, Force) ->
    case ets:lookup(?TABLE, Pid) of
        [Object] ->
            Sup = get_sup_from_user_id(Object),
            amoc_users_worker_sup:stop_children(Sup, [Pid], Force);
        _ ->
            ok
    end.

%% Group all children based on ID to their respective worker supervisor and cast a request with each
%% group at once. This way we reduce the number of casts to each worker to always one, instead of
%% depending on the number of users.
-spec start_children(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id(), any()) ->
    ok.
start_children(Scenario, StartId, EndId, ScenarioState) ->
    UserIds = lists:seq(StartId, EndId),
    Assignments = maps:groups_from_list(fun get_sup_from_user_id/1, UserIds),
    CastFun = fun(Sup, Users) ->
                      amoc_users_worker_sup:start_children(Sup, Scenario, Users, ScenarioState)
              end,
    maps:foreach(CastFun, Assignments).

%% Assign a count of children each worker needs to remove
%% in order to load-balance the request among all workers.
-spec stop_children(non_neg_integer(), boolean()) -> non_neg_integer().
stop_children(Count, Force) ->
    Users = case ets:match_object(?TABLE, '$1', Count) of
               '$end_of_table' ->
                   [];
               {Objects, _} ->
                   Objects
           end,
    stop_children_assignments(Users, Force),
    length(Users).

-spec get_all_children() -> [{pid(), amoc_scenario:user_id()}].
get_all_children() ->
    ets:tab2list(?TABLE).

-spec terminate_all_children() -> any().
terminate_all_children() ->
    Match = ets:match_object(?TABLE, '$1', 500),
    do_terminate_all_my_children(Match).

-spec stop_children_assignments([{pid(), amoc_scenario:user_id()}], boolean()) -> ok.
stop_children_assignments(Users, Force) ->
    Assignments = maps:groups_from_list(fun get_sup_from_user_id/1, fun get_pid/1, Users),
    CastFun = fun(Sup, Assignment) ->
                      amoc_users_worker_sup:stop_children(Sup, Assignment, Force)
              end,
    maps:foreach(CastFun, Assignments).

%% ets:continuation/0 type is unfortunately not exported from the ets module.
-spec do_terminate_all_my_children({[tuple()], term()} | '$end_of_table') -> ok.
do_terminate_all_my_children({Users, Continuation}) ->
    stop_children_assignments(Users, true),
    Match = ets:match_object(Continuation),
    do_terminate_all_my_children(Match);
do_terminate_all_my_children('$end_of_table') ->
    ok.

%% Helpers
-spec get_sup_from_user_id({pid(), amoc_scenario:user_id()} | amoc_scenario:user_id()) -> pid().
get_sup_from_user_id({_Pid, Id}) ->
    get_sup_from_user_id(Id);
get_sup_from_user_id(Id) ->
    #storage{sups = Supervisors, sups_count = SupCount} = persistent_term:get(?MODULE),
    Index = erlang:phash2(Id, SupCount) + 1,
    element(Index, Supervisors).

-spec get_pid({pid(), amoc_scenario:user_id()}) -> pid().
get_pid({Pid, _}) ->
    Pid.

-spec distribute(count(), assignment()) -> {count(), assignment()}.
distribute(Total, SupervisorsWithCounts) ->
    SupervisorWithPositiveCounts = [ T || T = {_, Count} <- SupervisorsWithCounts, Count =/= 0],
    Data = maps:from_list(SupervisorWithPositiveCounts),
    N = remove_n(Total, Data),
    distribute(#{}, Data, SupervisorWithPositiveCounts, Total, N).

-spec remove_n(count(), map()) -> non_neg_integer().
remove_n(Total, Data) when map_size(Data) > 0 ->
    case Total div map_size(Data) of
        0 -> 1;
        N -> N
    end;
remove_n(_Total, _Data) -> 0.

-spec distribute(#{pid() := count()}, #{pid() := count()}, assignment(), count(), count()) ->
    {count(), assignment()}.
%% Already assigned all, or not enough active users, we're done
distribute(Acc, Data, _, Left, _N) when 0 =:= Left; 0 =:= map_size(Data) ->
    {lists:sum(maps:values(Acc)), maps:to_list(Acc)};
%% Already assigned one round and still have counts left and running users available, loop again
distribute(Acc, Data, [], Left, _N) ->
    NewData = maps:filter(fun(_K, V) -> V > 0 end, Data),
    NewN = remove_n(Left, NewData),
    distribute(Acc, NewData, maps:to_list(NewData), Left, NewN);
distribute(Acc, Data, [{Sup, UsersInSup} | Rest], Left, N) ->
    case UsersInSup =< N of
        true ->
            %% Assigning the last possible users to this supervisor
            NewAcc = increment(Sup, UsersInSup, Acc),
            NewData = maps:put(Sup, 0, Data),
            distribute(NewAcc, NewData, Rest, Left - UsersInSup, N);
        false ->
            %% Assign N more to this supervisor and continue assigning
            NewAcc = increment(Sup, N, Acc),
            NewData = maps:put(Sup, UsersInSup - N, Data),
            distribute(NewAcc, NewData, Rest, Left - N, N)
    end.

increment(Key, Increment, Acc) ->
    maps:update_with(Key, fun(V) -> V + Increment end, Increment, Acc).
