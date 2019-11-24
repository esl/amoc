%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
stop() -> throw(normal_user_stop).

-spec stop(pid(), boolean()) -> no_return() | ok | {error, any()}.
stop(Pid, _Force) when Pid =:= self() ->
    stop();
stop(Pid, true) when is_pid(Pid) ->
    Node = node(Pid),
    supervisor:terminate_child({amoc_users_sup, Node}, Pid);
stop(Pid, false) when is_pid(Pid) ->
    exit(Pid, shutdown), %% do it in the same way as supervisor!!!
    ok.


-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) ->
    no_return().
init(Parent, Scenario, Id, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    R = try
            perform_scenario(Scenario, Id, State),
            normal
        catch
            throw:normal_user_stop ->
                normal;
            E:Reason:Stacktrace ->
                {E, Reason, Stacktrace}
        end,
    exit(R).

-spec perform_scenario(amoc:scenario(), amoc_scenario:user_id(), state()) -> ok.
perform_scenario(Scenario, Id, State) ->
    case erlang:function_exported(Scenario, start, 2) of
        true ->
            Scenario:start(Id, State);
        false ->
            Scenario:start(Id)
    end.
