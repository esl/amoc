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
         incr_no_of_users/0, decr_no_of_users/0, count_no_of_users/0,
         start_child/3, start_children/3, stop_children/2, terminate_all_children/0]).

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
    UserSups = supervisor:which_children(?MODULE),
    UserSupPids = [ Pid || {_, Pid, _, _} <- UserSups ],
    UserSupPidsTuple = erlang:list_to_tuple(UserSupPids),
    Atomic = atomics:new(1, [{signed, false}]),
    atomics:put(Atomic, 1, 0),
    Storage = #storage{user_count = Atomic, sups = UserSupPidsTuple},
    persistent_term:put(?MODULE, Storage).

-spec clean_storage() -> any().
clean_storage() ->
    persistent_term:erase(?MODULE).

-spec count_no_of_users() -> non_neg_integer().
count_no_of_users() ->
    #storage{user_count = Atomic} = persistent_term:get(?MODULE),
    atomics:get(Atomic, 1).

-spec incr_no_of_users() -> any().
incr_no_of_users() ->
    #storage{user_count = Atomic} = persistent_term:get(?MODULE),
    atomics:add(Atomic, 1, 1).

-spec decr_no_of_users() -> ok.
decr_no_of_users() ->
    #storage{user_count = Atomic} = persistent_term:get(?MODULE),
    case atomics:sub_get(Atomic, 1, 1) of
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
    TotalCount = count_no_of_users(),
    #storage{sups = Sups} = persistent_term:get(?MODULE),
    NumOfSupervisors = tuple_size(Sups),
    ShuffledSupervisors = shuffle_supervisors(tuple_to_list(Sups)),
    CountRemove = min(Count, TotalCount),
    Assignments = assign_counts_to_supervisors(ShuffledSupervisors, NumOfSupervisors, CountRemove),
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

%% Create a list with random indexes with supervisors that will then be sorted
%% Uses mwc59 for the most performant (though statistically unsound) erlang RNG
-spec shuffle_supervisors([pid()]) -> [pid()].
shuffle_supervisors(Supervisors) ->
    UniqueInt = erlang:unique_integer([positive]),
    F1 = fun(Sup, Int) ->
                Rand = rand:mwc59(Int),
                {{Rand, Sup}, Rand}
        end,
    {Indexed, _} = lists:mapfoldl(F1, UniqueInt, Supervisors),
    SortedByIndex = lists:sort(Indexed),
    {_Indexes, ShuffledSupervisors} = lists:unzip(SortedByIndex),
    ShuffledSupervisors.

-spec assign_counts_to_supervisors([pid()], non_neg_integer(), non_neg_integer()) ->
    [{pid(), non_neg_integer()}].
assign_counts_to_supervisors(ShuffledList, NumOfSupervisors, CountRemove) ->
    SlotPerSup = CountRemove div NumOfSupervisors,
    Remainder = CountRemove rem NumOfSupervisors,
    F2 = fun(Sup, Rem) ->
                 {{Sup, SlotPerSup + Rem}, min(0, Rem - 1)}
         end,
    {Assignments, _} = lists:mapfoldl(F2, Remainder, ShuffledList),
    Assignments.
