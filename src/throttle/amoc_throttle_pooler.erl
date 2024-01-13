%% @private
%% @see amoc_throttle
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_throttle_pooler).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ChildSpec = #{id => amoc_throttle_pool,
                  start => {amoc_throttle_pool, start_link, []},
                  type => supervisor,
                  shutdown => infinity,
                  restart => transient,
                  modules => [amoc_throttle_pool] },
    SupFlags = #{strategy => simple_one_for_one, intensity => 0},
    {ok, {SupFlags, [ChildSpec]}}.
