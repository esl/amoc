%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc).

-define(INTERARRIVAL_DEFAULT, 30).
-define(INTERARRIVAL,
    application:get_env(amoc, interarrival, ?INTERARRIVAL_DEFAULT)).

-export([do/1]).

do([Scenario, Count]) ->
    C = list_to_integer(atom_to_list(Count)),
    start_users(Scenario, 1, C);
do([Scenario, Start, End]) ->
    application:ensure_all_started(amoc),
    case code:ensure_loaded(Scenario) of
        {module, Scenario} ->
            ok;
        Error ->
            lager:error("scenario module ~p cannot be found, reaso: ~p", [Scenario, Error]),
            exit(1)
    end,
    lager:info("starting scenario begin_id=~p, end_id=~p", [Start, End]),
    S = list_to_integer(atom_to_list(Start)),
    E = list_to_integer(atom_to_list(End)),
    init_scenario(Scenario),
    start_users(Scenario, S, E).

start_users(Scenario, S,E) ->
    [ start_user(Scenario, Id) || Id <- lists:seq(S,E) ].

start_user(Scenario, Id) ->
    R = supervisor:start_child(amoc_users_sup, [Scenario, Id]),
    timer:sleep(?INTERARRIVAL),
    R.

init_scenario(Scenario) ->
    case erlang:function_exported(Scenario, init, 0) of
        true ->
            Scenario:init();
        _ ->
            skip
    end.
