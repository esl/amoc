%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_users_sup).
-behaviour(supervisor).


-export([start_link/0, stop_child/2, stop_children/2]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SHUTDOWN_TIMEOUT, 2000). %% 2 seconds

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec stop_child(pid(), boolean()) -> ok | {error, any()}.
stop_child(Pid, true) ->
    Node = node(Pid),
    supervisor:terminate_child({amoc_users_sup, Node}, Pid);
stop_child(Pid, false) when is_pid(Pid) ->
    exit(Pid, shutdown), %% do it in the same way as supervisor!!!
    ok.

-spec stop_children([pid()], boolean()) -> ok.
stop_children(Pids, false) ->
    [stop_child(Pid, false) || Pid <- Pids],
    ok;
stop_children(Pids, true) ->
    %% stopping users one by one using supervisor:terminate_child/2 is
    %% not an option because terminate_child requests are queued and
    %% processed by supervisor sequentially. and if user process ignores
    %% exit(Child,shutdown) signal that causes ?SHUTDOWN_TIMEOUT delay
    %% before it's killed by exit(Child,kill). so attempt to remove N
    %% users may take take N*?SHUTDOWN_TIMEOUT milliseconds, which is
    %% not acceptable. so let's do the same thing as supervisor but in
    %% parallel, so it won't result in huge delay.
    spawn(
        fun() ->
            [exit(Pid, shutdown) || Pid <- Pids],
            timer:sleep(?SHUTDOWN_TIMEOUT),
            [exit(Pid, kill) || Pid <- Pids]
        end),
    ok.

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
               shutdown => ?SHUTDOWN_TIMEOUT,
               type => worker,
               modules => [amoc_user]},

    {ok, {SupFlags, [AChild]}}.
