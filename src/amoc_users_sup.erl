%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_users_sup).
-behaviour(supervisor).


-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} | ignore | {error, Reason :: term()}).

init([]) ->
    ets:new(amoc_users, [named_table, public,
                         {write_concurrency, true},
                         {read_concurrency, true}]),
    process_flag(priority, max),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {amoc_user, {amoc_user, start_link, []},
        Restart, Shutdown, Type, [amoc_user]},

    {ok, {SupFlags, [AChild]}}.
