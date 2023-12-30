%% @private
%% @see amoc_coordinator
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc Global supervisor for all started coordination actions
-module(amoc_coordinator_sup).

-behaviour(supervisor).

-export([start_coordinator/3, stop_coordinator/1, get_workers/1]).

-export([start_link/0, init/1]).

-spec start_coordinator(amoc_coordinator:name(), amoc_coordinator:coordination_plan(), timeout()) ->
    {ok, pid()} | {error, term()}.
start_coordinator(Name, Plan, Timeout) ->
    case supervisor:start_child(?MODULE, [{Name, Plan, Timeout}]) of
        {ok, Coordinator} ->
            Children = supervisor:which_children(Coordinator),
            [TimeoutWorker] = [ Pid || {{amoc_coordinator_timeout, _, _}, Pid, _, _} <- Children ],
            Workers = [ Pid || {{amoc_coordinator_worker, _}, Pid, _, _} <- Children ],
            store_coordinator(Name, Coordinator, TimeoutWorker, Workers),
            {ok, Coordinator};
        Other ->
            Other
    end.

-spec stop_coordinator(amoc_coordinator:name()) ->
    ok | {error, term()}.
stop_coordinator(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_, #{coordinator := Coordinator, timeout_worker := TimeoutWorker, workers := Workers}}] ->
            [ amoc_coordinator_worker:stop(Worker) || Worker <- Workers ],
            erlang:send(TimeoutWorker, terminate),
            supervisor:terminate_child(?MODULE, Coordinator);
        [] ->
            {error, not_found}
    end.

-spec get_workers(amoc_coordinator:name()) ->
    {ok, pid(), [pid()]} | {error, term()}.
get_workers(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_, #{timeout_worker := TimeoutWorker, workers := Workers}}] ->
            {ok, TimeoutWorker, Workers};
        [] ->
            {error, not_found}
    end.

store_coordinator(Name, Coordinator, TimeoutWorker, Workers) ->
    Item = #{coordinator => Coordinator, timeout_worker => TimeoutWorker, workers => Workers},
    ets:insert(?MODULE, {Name, Item}).

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ets:new(?MODULE,  [named_table, ordered_set, public, {read_concurrency, true}]),
    SupFlags = #{strategy => simple_one_for_one},
    AChild = #{id => amoc_coordinator_worker_sup,
               start => {amoc_coordinator_worker_sup, start_link, []},
               restart => transient,
               shutdown => infinity,
               type => supervisor,
               modules => [amoc_coordinator_worker_sup]},
    {ok, {SupFlags, [AChild]}}.
