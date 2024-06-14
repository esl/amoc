%% @private
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 0},
          [
              ?SUP(amoc_users_sup),
              ?SUP(amoc_throttle_sup),
              ?SUP(amoc_coordinator_sup),
              ?WORKER(amoc_controller),
              ?WORKER(amoc_cluster),
              ?WORKER(amoc_code_server)
          ]}}.
