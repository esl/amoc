%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
    amoc_users = start_users_ets(),
    {ok, { {one_for_one, 5, 10},
           [
            ?CHILD(amoc_event, worker),
            ?CHILD(amoc_users_sup, supervisor),
            ?CHILD(amoc_controller, worker),
            ?CHILD(amoc_slave, worker)
           ]} }.

-spec start_users_ets() -> ets:tid() | atom().
start_users_ets() ->
    ets:new(amoc_users, [named_table,
                         ordered_set,
                         public,
                         {write_concurrency, true},
                         {read_concurrency, true}]).
