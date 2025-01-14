%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_throttle_pool).

-behaviour(supervisor).

-export([get_workers/1]).
-export([start_link/2, init/1]).

-spec get_workers(pid()) -> #{non_neg_integer() := pid()}.
get_workers(PoolSup) ->
    Processes = supervisor:which_children(PoolSup),
    Workers = [ {N, Pid} || {{amoc_throttle_process, N}, Pid, _, _} <- Processes, is_pid(Pid) ],
    maps:from_list(Workers).

-spec start_link(amoc_throttle:name(), amoc_throttle_config:pool_config()) ->
    supervisor:startlink_ret().
start_link(Name, PoolConfig) ->
    supervisor:start_link(?MODULE, {Name, PoolConfig}).

-spec init({amoc_throttle:name(), amoc_throttle_config:pool_config()}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Name, ConfigPerProcess}) ->
    Children = [
                #{id => {amoc_throttle_process, N},
                  start => {amoc_throttle_process, start_link, [Name, MaxN, Delay]},
                  type => worker,
                  shutdown => timer:seconds(60),
                  restart => transient,
                  modules => [amoc_throttle_process]
                 }
                || {N, #{max_n := MaxN, delay := Delay}} <- maps:to_list(ConfigPerProcess)
               ],
    SupFlags = #{strategy => one_for_one, intensity => 0},
    {ok, {SupFlags, Children}}.
