%% @private
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc Top supervisor of the pooled users supervisor.
%%
%% It spawns a pool of workers as big as online schedulers. When starting a new user, as the user is
%% identified by ID, a worker will be chosen for this user based on its ID
%% (see get_sup_for_user_id/1).
%%
%% The currently running number of users is stored in an atomic that all workers update and the
%% controller can read.
-module(amoc_users_sup).

-behaviour(supervisor).

%% Supervisor
-export([start_link/0, init/1]).

%% API
-export([init_storage/0, clean_storage/0,
         incr_no_of_users/1, decr_no_of_users/1, count_no_of_users/0,
         start_child/3, start_children/3, stop_children/2, terminate_all_children/0]).

-type count() :: non_neg_integer().

-record(storage, {
          user_count :: atomics:atomics_ref(),
          sups :: tuple()
         }).

%% Supervisor

%% @private
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

%% @private
-spec init(no_args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(no_args) ->
    Specs = [
             #{
               id => {amoc_users_worker_sup, N},
               start => {amoc_users_worker_sup, start_link, [N]},
               restart => permanent,
               shutdown => infinity,
               type => worker,
               modules => [amoc_users_worker_sup]
              }
             || N <- lists:seq(1, erlang:system_info(schedulers_online)) ],
    Strategy = #{strategy => one_for_one, intensity => 0},
    {ok, {Strategy, Specs}}.

%% API
-spec init_storage() -> any().
init_storage() ->
    UserSups = lists:reverse(supervisor:which_children(?MODULE)),
    UserSupPids = [ Pid || {_, Pid, _, _} <- UserSups ],
    UserSupPidsTuple = erlang:list_to_tuple(UserSupPids),
    NumOfSupervisors = tuple_size(UserSupPidsTuple),
    Atomics = atomics:new(1 + NumOfSupervisors, [{signed, false}]),
    Storage = #storage{user_count = Atomics, sups = UserSupPidsTuple},
    persistent_term:put(?MODULE, Storage).

-spec clean_storage() -> any().
clean_storage() ->
    persistent_term:erase(?MODULE).

-spec count_no_of_users() -> count().
count_no_of_users() ->
    #storage{user_count = Atomics} = persistent_term:get(?MODULE),
    atomics:get(Atomics, 1).

-spec incr_no_of_users(non_neg_integer()) -> any().
incr_no_of_users(SupNum) ->
    #storage{user_count = Atomics} = persistent_term:get(?MODULE),
    atomics:add(Atomics, 1, 1),
    atomics:add(Atomics, SupNum + 1, 1).

-spec decr_no_of_users(non_neg_integer()) -> ok.
decr_no_of_users(SupNum) ->
    #storage{user_count = Atomics} = persistent_term:get(?MODULE),
    atomics:sub(Atomics, SupNum + 1, 1),
    case atomics:sub_get(Atomics, 1, 1) of
        0 ->
            amoc_controller:zero_users_running();
        _ ->
            ok
    end.

-spec start_child(amoc:scenario(), amoc_scenario:user_id(), any()) -> ok.
start_child(Scenario, Id, ScenarioState) ->
    Sup = get_sup_for_user_id(Id),
    gen_server:cast(Sup, {start_child, Scenario, Id, ScenarioState}).

%% Group all children based on ID to their respective worker supervisor and cast a request with each
%% group at once. This way we reduce the number of casts to each worker to always one, instead of
%% depending on the number of users.
-spec start_children(amoc:scenario(), [amoc_scenario:user_id()], any()) -> ok.
start_children(Scenario, UserIds, ScenarioState) ->
    KeyFun = fun(UserId) ->
                     get_sup_for_user_id(UserId)
             end,
    Assignments = maps:groups_from_list(KeyFun, UserIds),
    CastFun = fun (Sup, Users) ->
                      gen_server:cast(Sup, {start_children, Scenario, Users, ScenarioState})
              end,
    maps:foreach(CastFun, Assignments).

%% Assign a count of children each worker needs to remove
%% in order to load-balance the request among all workers.
-spec stop_children(non_neg_integer(), boolean()) -> non_neg_integer().
stop_children(Count, Force) ->
    {CountRemove, Assignments} = assign_counts(Count),
    [ gen_server:cast(Sup, {stop_children, Int, Force}) || {Sup, Int} <- Assignments ],
    CountRemove.

-spec terminate_all_children() -> any().
terminate_all_children() ->
    #storage{sups = Sups} = persistent_term:get(?MODULE),
    [ gen_server:cast(Sup, terminate_all_children) || Sup <- tuple_to_list(Sups) ].

%% Helpers
-spec get_sup_for_user_id(amoc_scenario:user_id()) -> pid().
get_sup_for_user_id(Id) ->
    #storage{sups = Supervisors} = persistent_term:get(?MODULE),
    Index = Id rem tuple_size(Supervisors) + 1,
    element(Index, Supervisors).

%% assign how many users each worker will be requested to remove,
%% taking care of the fact that worker might not have enough users
assign_counts(Total) ->
    #storage{user_count = Atomics, sups = Sups} = persistent_term:get(?MODULE),
    NumOfSupervisors = tuple_size(Sups),
    Supervisors = tuple_to_list(Sups),
    UsersPerSup = [ atomics:get(Atomics, SupPos) || SupPos <- lists:seq(2, NumOfSupervisors + 1) ],
    SupervisorsWithCounts = lists:zip(Supervisors, UsersPerSup),
    SupervisorWithPositiveCounts = [ T || T = {_, Count} <- SupervisorsWithCounts, Count =/= 0],
    Data = maps:from_list(SupervisorWithPositiveCounts),
    distribute(#{}, Data, SupervisorWithPositiveCounts, Total).

-spec distribute(#{pid() := count()}, #{pid() := count()}, [{pid(), count()}], count()) ->
    {count(), [{pid(), count()}]}.
%% Assigned all or not enough active users, already assigned all possible
distribute(Acc, Data, _, Left) when 0 =:= Left; 0 =:= map_size(Data) ->
    {lists:sum(maps:values(Acc)), maps:to_list(Acc)};
%% Already assigned one round and still have counts left and running users available, loop again
distribute(Acc, Data, [], Left) ->
    distribute(Acc, Data, maps:to_list(Data), Left);
distribute(Acc, Data, [{Sup, Count} | Rest], Left) ->
    NewAcc = maps:put(Sup, maps:get(Sup, Acc, 0) + 1, Acc),
    NewData = case Count of
                  1 ->
                      %% Assigning last possible user to this sup, remove from data
                      maps:remove(Sup, Data);
                  _ ->
                      %% Assign one more to this sup and continue assigning
                      maps:put(Sup, Count - 1, Data)
              end,
    distribute(NewAcc, NewData, Rest, Left - 1).
