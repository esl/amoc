%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_users_sup).
-behaviour(supervisor).


-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    process_flag(priority, max),

    SupFlags = #{strategy => simple_one_for_one},

    AChild = #{id => amoc_user,
               start => {amoc_user, start_link, []},
               %% A temporary child process is never restarted
               restart => temporary,
               %% sending exit(Child,shutdown) first. If a process doesn't stop within
               %% the shutdown timeout, than killing it brutally with exit(Child,kill).
               %% don't make this too big, as supervisor:terminate_child/2 is a blocking
               %% gen_server call, so removing N users will take N*Delay milliseconds.
               shutdown => 500,
               type => worker,
               modules => [amoc_user]},

    {ok, {SupFlags, [AChild]}}.
