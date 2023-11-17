%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_user).

%% API
-export([start_link/3]).
-export([stop/0, stop/2]).
-export([init/4]).

-type state() :: term().

-spec start_link(amoc:scenario(), amoc_scenario:user_id(), state()) ->
    {ok, pid()}.
start_link(Scenario, Id, State) ->
    proc_lib:start_link(?MODULE, init, [self(), Scenario, Id, State]).

-spec stop() -> no_return().
stop() ->
    stop(self(), false).

-spec stop(pid(), boolean()) -> no_return() | ok | {error, any()}.
stop(Pid, Force) when is_pid(Pid) ->
    amoc_users_sup:stop_child(Pid, Force).

-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) ->
    no_return().
init(Parent, Scenario, Id, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    ScenarioFun = fun() -> {amoc_scenario:start(Scenario, Id, State), #{}} end,
    telemetry:span([amoc, scenario, user], #{}, ScenarioFun).
