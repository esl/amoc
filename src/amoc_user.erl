%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_user).

%% defaults
-define(REPEAT_INTERVAL, 60000). % time between user restarts (60s)
-define(REPEAT_NUM, infinity). % number of scenario repetitions

%% API
-export([start_link/3, stop/0]).
-export([init/4]).

-type state() :: term().

-spec start_link(amoc:scenario(), amoc_scenario:user_id(), state()) ->
    {ok, pid()}.
start_link(Scenario, Id, State) ->
    proc_lib:start_link(?MODULE, init, [self(), Scenario, Id, State]).

stop() -> throw(normal_user_stop).

-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) ->
    no_return().
init(Parent, Scenario, Id, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    ets:insert(amoc_users, {Id, self()}),
    F = fun() -> perform_scenario(Scenario, Id, State) end,
    R = try
            case repeat_num() of
                infinity -> repeat(F);
                N -> repeat(F, N)
            end,
            normal
        catch
            throw:normal_user_stop ->
                normal;
            E:Reason:Stacktrace ->
                {E, {abnormal_exit, Reason}, Stacktrace}
        after
            ets:delete(amoc_users, Id)
        end,
    exit(R).

-spec perform_scenario(amoc:scenario(), amoc_scenario:user_id(), state()) -> ok.
perform_scenario(Scenario, Id, State) ->
    case erlang:function_exported(Scenario, start, 2) of
        true ->
            Scenario:start(Id, State);
        false ->
            Scenario:start(Id)
    end,
    flush_mailbox().

-spec flush_mailbox() -> ok.
flush_mailbox() ->
    receive
        _ ->
            flush_mailbox()
    after 0 ->
        ok
    end.

repeat(F) ->
    F(),
    timer:sleep(repeat_interval()),
    repeat(F).

repeat(F, N) when N > 1 ->
    F(),
    timer:sleep(repeat_interval()),
    repeat(F, N - 1);
repeat(F, 1) ->
    F().

repeat_interval() ->
    amoc_config_env:get(repeat_interval, ?REPEAT_INTERVAL).

repeat_num() ->
    amoc_config_env:get(repeat_num, ?REPEAT_NUM).
