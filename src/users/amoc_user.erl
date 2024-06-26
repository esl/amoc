%% @private
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_user).

%% API
-export([start_link/3]).
-export([stop/0, stop/2]).
-export([init/4]).

-type state() :: term().

-spec start_link(amoc:scenario(), amoc_scenario:user_id(), state()) ->
    {ok, pid()} | {error, term()}.
start_link(Scenario, Id, State) ->
    proc_lib:start_link(?MODULE, init, [self(), Scenario, Id, State]).

-spec stop() -> ok.
stop() ->
    stop(self(), false).

-spec stop(pid(), boolean()) -> ok.
stop(Pid, Force) when is_pid(Pid) ->
    amoc_users_sup:stop_child(Pid, Force).

-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) -> term().
init(Parent, Scenario, Id, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    amoc_scenario:start(Scenario, Id, State).
