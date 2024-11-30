%% @copyright 2023 Erlang Solutions Ltd.
%% @doc API module for running locally, in a non-distributed environment
%%
%% Use `amoc_dist' module to run scenarios in a distributed environment
%% @end
-module(amoc).

-export([do/3,
         add/1,
         remove/2,
         stop/0,
         reset/0]).

-type scenario() :: module().
-export_type([scenario/0]).

%% @doc Start a scenario with the given number of users and configuration
-spec do(scenario(), non_neg_integer(), amoc_config:settings()) ->
    ok | {error, term()}.
do(Scenario, Count, Settings) ->
    case amoc_cluster:set_master_node(node()) of
        ok ->
            %% amoc_controller:start_scenario/2 will fail,
            %% if amoc is running in a distributed mode
            case {amoc_controller:start_scenario(Scenario, Settings), Count} of
                {ok, 0} -> ok;
                {ok, Count} -> amoc_controller:add_users(1, Count);
                {Error, _} -> Error
            end;
        Error -> Error
    end.

%% @doc Dynamically add more users to a currently running scenario
-spec add(pos_integer()) -> ok | {error, any()}.
add(Count) when is_integer(Count), Count > 0 ->
    case is_running_locally() of
        ok ->
            {running, #{highest_user_id := LastUserId}} = amoc_controller:get_status(),
            amoc_controller:add_users(LastUserId + 1, LastUserId + Count);
        Error -> Error
    end.

%% @doc Dynamically remove more users from a currently running scenario, optionally forcibly
%%
%% Forcing user removal means that all users will be signal to exit in parallel,
%% and will forcibly be killed after a short timeout (2 seconds),
%% whether they have exited already or not.
-spec remove(pos_integer(), boolean()) -> {ok, non_neg_integer()} | {error, any()}.
remove(Count, ForceRemove) when is_integer(Count), Count > 0 ->
    case is_running_locally() of
        ok ->
            amoc_controller:remove_users(Count, ForceRemove);
        Error -> Error
    end.

%% @doc Stop a running scenario
-spec stop() -> ok | {error, any()}.
stop() ->
    case is_running_locally() of
        ok ->
            amoc_controller:stop_scenario();
        Error -> Error
    end.

%% @doc Restart the whole amoc supervision tree
-spec reset() -> ok | {error, term()}.
reset() ->
    case is_running_locally() of
        ok ->
            application:stop(?MODULE),
            application:ensure_all_started(?MODULE),
            ok;
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% Local functions
%% ------------------------------------------------------------------

-spec is_running_locally() -> ok | {error, any()}.
is_running_locally() ->
    Node = node(),
    case {amoc_cluster:master_node(), amoc_controller:get_status()} of
        {undefined, _} -> {error, master_node_is_not_set};
        {Node, disabled} -> {error, node_is_clustered};
        {Node, _} -> ok;
        {_, _} -> {error, slave_node}
    end.
