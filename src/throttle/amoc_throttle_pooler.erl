%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_throttle_pooler).

-behaviour(supervisor).

-export([start_pool/2, stop_pool/1]).
-export([start_link/0, init/1]).

-spec start_pool(amoc_throttle:name(), amoc_throttle_config:pool_config()) ->
    supervisor:startchild_ret().
start_pool(Name, PoolConfig) ->
    supervisor:start_child(amoc_throttle_pooler, [Name, PoolConfig]).

-spec stop_pool(pid()) -> ok.
stop_pool(Pool) ->
    ok = supervisor:terminate_child(amoc_throttle_pooler, Pool).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ChildSpec = #{id => amoc_throttle_pool,
                  start => {amoc_throttle_pool, start_link, []},
                  type => supervisor,
                  shutdown => infinity,
                  restart => transient,
                  modules => [amoc_throttle_pool] },
    SupFlags = #{strategy => simple_one_for_one, intensity => 0},
    {ok, {SupFlags, [ChildSpec]}}.
