%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_user).

%% time between sceanario restarts
-define(REPEAT_INTERVAL_DEFAULT, 60000). % 60s
-define(REPEAT_INTERVAL,
    application:get_env(amoc, repeat_interval, ?REPEAT_INTERVAL_DEFAULT)).

-define(REPEAT_NUM_DEFAULT, infinity).
-define(REPEAT_NUM,
    application:get_env(amoc, repeat_num, ?REPEAT_NUM_DEFAULT)).

%% API
-export([start_link/3]).
-export([init/4]).

-type state() :: term().

-spec start_link(amoc:scenario(), amoc_scenario:user_id(), state()) ->
    {ok, pid()}.
start_link(Scenario, Id, State) ->
    proc_lib:start_link(?MODULE, init, [self(), Scenario, Id, State]).

-spec init(pid(), amoc:scenario(), amoc_scenario:user_id(), state()) ->
    no_return().
init(Parent, Scenario, Id, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    ets:insert(amoc_users, {Id, self()}),
    R = try
            repeat(Scenario, Id, State, ?REPEAT_NUM),
            normal
        catch
            throw:stop ->
                normal;
            %% {R, get_stack()} will result in a compact error message
            %% {E, R, get_stack()} will result in a full stack report
            E:Reason ->
                {E, {abnormal_exit, Reason}, erlang:get_stacktrace()}
        after
            ets:delete(amoc_users, Id)
        end,
    exit(R).

-spec repeat(amoc:scenario(), amoc_scenario:user_id(), state(),
             infinity | pos_integer()) -> ok.
repeat(Scenario, Id, State, Repetitions) ->
    case erlang:function_exported(Scenario, start, 2) of
        true ->
            Scenario:start(Id, State);
        false ->
            Scenario:start(Id)
    end,
    flush_mailbox(),
    perform_next_repetition(Scenario, Id, State, Repetitions).

perform_next_repetition(_Scenario, _Id, _State, 1) ->
    ok;
perform_next_repetition(Scenario, Id, State, infinity) ->
    timer:sleep(?REPEAT_INTERVAL),
    repeat(Scenario, Id, State, infinity);
perform_next_repetition(Scenario, Id, State, N) when N > 0 ->
    timer:sleep(?REPEAT_INTERVAL),
    repeat(Scenario, Id, State, N - 1).

-spec flush_mailbox() -> ok.
flush_mailbox() ->
    receive
        stop ->
            throw(stop);
        _ ->
            flush_mailbox()
    after 0 ->
        ok
    end.
