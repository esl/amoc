%% @private
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUP(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: pos_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }}.
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [
              ?SUP(amoc_users_sup, supervisor),
              ?SUP(amoc_throttle_sup, supervisor),
              ?SUP(amoc_coordinator_sup, supervisor),
              ?WORKER(amoc_controller, worker),
              ?WORKER(amoc_cluster, worker),
              ?WORKER(amoc_code_server, worker)
          ]}}.
