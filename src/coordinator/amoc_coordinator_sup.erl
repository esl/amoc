%% @private
%% @see amoc_coordinator
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Global supervisor for all started coordination actions
-module(amoc_coordinator_sup).

-behaviour(supervisor).

-export([start_coordinator/3, stop_coordinator/1, get_workers/1]).

-export([start_link/0, init/1]).

-spec start_coordinator(amoc_coordinator:name(), amoc_coordinator:plan(), timeout()) ->
    {ok, pid()} | {error, term()}.
start_coordinator(Name, OrderedPlan, Timeout) ->
    case supervisor:start_child(?MODULE, [{Name, OrderedPlan, Timeout}]) of
        {ok, Coordinator} ->
            Children = supervisor:which_children(Coordinator),
            Workers = get_workers_in_order(Name, OrderedPlan, Children),
            TimeoutWorker = get_timeout_pid(Name, Timeout, Children),
            store_coordinator(Name, Coordinator, TimeoutWorker, Workers),
            {ok, Coordinator};
        Other ->
            Other
    end.

-spec get_workers_in_order(amoc_coordinator:name(), amoc_coordinator:plan(), []) -> [pid()].
get_workers_in_order(Name, OrderedPlan, Children) ->
    [ get_child_pid({amoc_coordinator_worker, Name, Item}, Children) || Item <- OrderedPlan ].

-spec get_timeout_pid(amoc_coordinator:name(), amoc_coordinator:plan(), []) -> pid().
get_timeout_pid(Name, Timeout, Children) ->
    get_child_pid({amoc_coordinator_timeout, Name, Timeout}, Children).

get_child_pid(ChildId, Children) ->
    [Pid] = [ Pid || {Id, Pid, _, _} <- Children, Id =:= ChildId ],
    Pid.

-spec stop_coordinator(amoc_coordinator:name()) ->
    ok | {error, term()}.
stop_coordinator(Name) ->
    case persistent_term:get({?MODULE, Name}) of
        #{coordinator := Coordinator, timeout_worker := TimeoutWorker, workers := Workers} ->
            amoc_coordinator_timeout:stop(TimeoutWorker),
            [ amoc_coordinator_worker:stop(Worker) || Worker <- Workers ],
            supervisor:terminate_child(?MODULE, Coordinator);
        not_found ->
            {error, not_found}
    end.

-spec get_workers(amoc_coordinator:name()) ->
    {ok, pid(), [pid()]} | {error, term()}.
get_workers(Name) ->
    case persistent_term:get({?MODULE, Name}, not_found) of
        #{timeout_worker := TimeoutWorker, workers := Workers} ->
            {ok, TimeoutWorker, Workers};
        not_found ->
            {error, not_found}
    end.

store_coordinator(Name, Coordinator, TimeoutWorker, Workers) ->
    Item = #{coordinator => Coordinator, timeout_worker => TimeoutWorker, workers => Workers},
    persistent_term:put({?MODULE, Name}, Item).

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    AChild = #{id => amoc_coordinator_worker_sup,
               start => {amoc_coordinator_worker_sup, start_link, []},
               restart => transient,
               shutdown => infinity,
               type => supervisor,
               modules => [amoc_coordinator_worker_sup]},
    SupFlags = #{strategy => simple_one_for_one, intensity => 0},
    {ok, {SupFlags, [AChild]}}.
