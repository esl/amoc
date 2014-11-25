%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_user).

%% time between sceanario restarts
-define(REPEAT_INTERVAL_DEFAULT, 60000). % 60s
-define(REPEAT_INTERVAL,
    application:get_env(amoc, repeat_interval, ?REPEAT_INTERVAL_DEFAULT)).

%% API
-export([start_link/2]).
-export([init/3]).


start_link(Scenario, Id) ->
    proc_lib:start_link(?MODULE, init, [self(), Scenario, Id]).

init(Parent, Scenario, Id) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    ets:insert(amoc_users, {Id, self()}),
    R = try
        forever(Scenario, Id),
        normal
    catch
        %% {R, get_stack()} will result in a compact error message
        %% {E, R, get_stack()} will result in a full stack report
        E:Reason -> {E, Reason, erlang:get_stacktrace()}
    after
        ets:delete(amoc_users, Id)
    end,
    exit(R).

forever(Scenario, Id) ->
    Scenario:start(Id),
    flush_mailbox(),
    timer:sleep(?REPEAT_INTERVAL),
    forever(Scenario, Id).

flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.
