%% @private
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_user).

%% API
-export([stop/0, stop/2]).
-export([init/4]).

-type state() :: term().

-spec stop() -> ok.
stop() ->
    stop(self(), false).

-spec stop(pid(), boolean()) -> ok.
stop(Pid, Force) when is_pid(Pid) ->
    amoc_users_sup:stop_child(Pid, Force).

-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) -> term().
init(Parent, Scenario, Id, State) ->
    amoc_controller:wait_user_rate(),
    amoc_users_worker_sup:user_up(Parent, Id),
    process_flag(trap_exit, true),
    amoc_scenario:start(Scenario, Id, State).
