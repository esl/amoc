%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Top supervisor for the controller, the pooler, and the process group
%%
%% The supervision tree is as follows
%%
%%                      amoc_sup
%%                         |
%%                 amoc_throttle_sup
%%                 /       |        \
%%                /        |         \
%%               /         |          \
%%           pooler    controller      pg
%%             ||
%%        (dynamically)
%%             ||
%%            pool
%%           /  |  \
%%         [processes()]
%%
%% Where the pool, on creation, subscribes all its children to the named process group
%% @end
-module(amoc_throttle_sup).

-behaviour(supervisor).

-define(PG_SCOPE, amoc_throttle).

-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one},
    Pg = #{id => pg,
           start => {pg, start_link, [?PG_SCOPE]},
           type => worker,
           shutdown => timer:seconds(5),
           restart => permanent,
           modules => [pg]},
    Controller = #{id => amoc_throttle_controller,
                   start => {amoc_throttle_controller, start_link, []},
                   type => worker,
                   shutdown => timer:seconds(5),
                   restart => permanent,
                   modules => [amoc_throttle_controller]},
    Pooler = #{id => amoc_throttle_pooler,
               start => {amoc_throttle_pooler, start_link, []},
               type => supervisor,
               shutdown => infinity,
               restart => permanent,
               modules => [amoc_throttle_pooler]},
    {ok, {SupFlags, [Pg, Controller, Pooler]}}.
