%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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
              ?CHILD(amoc_users_sup, supervisor),
              ?CHILD(amoc_controller, worker),
              ?CHILD(amoc_cluster, worker),
              ?CHILD(amoc_scenario, worker),
              ?CHILD(amoc_throttle_controller, worker)
          ]}}.


