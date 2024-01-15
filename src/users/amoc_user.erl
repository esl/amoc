%% @private
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_user).

-define(SHUTDOWN_TIMEOUT, 2000). %% 2 seconds

%% API
-export([start_link/3]).
-export([stop/2]).
-export([init/4]).

-type state() :: term().

-spec start_link(amoc:scenario(), amoc_scenario:user_id(), state()) ->
    {ok, pid()} | {error, term()}.
start_link(Scenario, Id, State) ->
    proc_lib:start_link(?MODULE, init, [self(), Scenario, Id, State]).

-spec stop(pid(), boolean()) -> no_return() | ok | {error, any()}.
stop(Pid, false) when is_pid(Pid), Pid =/= self() ->
    proc_lib:stop(Pid, shutdown, ?SHUTDOWN_TIMEOUT);
stop(Pid, true) when is_pid(Pid), Pid =/= self() ->
    proc_lib:stop(Pid, kill, ?SHUTDOWN_TIMEOUT).

-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) ->
    no_return().
init(Parent, Scenario, Id, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    amoc_scenario:start(Scenario, Id, State).
