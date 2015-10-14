%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc).
-define(INTERARRIVAL_DEFAULT, 30).
-define(INTERARRIVAL,
    application:get_env(amoc, interarrival, ?INTERARRIVAL_DEFAULT)).

-export([do/1]).

-export_type([
              scenario/0,
              do_opts/0,
              remove_opts/0
             ]).

-type scenario() :: module().

-type do_opt() :: {nodes, [node()]} | {comment, string()} | {repeat, timeout()} |
                  {interarrival, timeout()}.
-type do_opts() :: [do_opt()].

-type remove_opt() :: {force, boolean()}.
-type remove_opts() :: [remove_opt()].

%% ------------------------------------------------------------------
%% Start scenario via shell script (run.sh)
%% ------------------------------------------------------------------
-spec do([scenario() | integer(), ...]) -> ok | no_return().
do([Scenario, Count]) ->
    do([Scenario, '1', Count]);
do([Scenario, Start, End]) ->
    application:ensure_all_started(amoc),
    S = list_to_integer(atom_to_list(Start)),
    E = list_to_integer(atom_to_list(End)),
    case amoc_local:do(Scenario, S, E) of
        ok ->
            ok;
        {error, _} ->
            exit(1)
    end.
