%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_dist).

-export([start_nodes/0,
         do/3,
         do/4,
         add/1,
         add/2,
         remove/2,
         remove/3]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
start_nodes() ->
    Hosts = application:get_env(amoc, hosts, []),
    Path = application:get_env(amoc, path, "/usr"),
    start_nodes(Hosts, Path).

do(Scenario, Start, End) ->
    do(Scenario, Start, End, []).

do(Scenario, Start, End, Opts) ->
    Nodes = proplists:get_value(nodes, Opts, nodes()),
    Comment = proplists:get_value(comment, Opts, "none"),

    InterArrival = proplists:get_value(interarrival, Opts, 75),
    RepeatTimeout = proplists:get_value(repeat, Opts, 75),
    Step = proplists:get_value(step, Opts, 1),

    amoc_event:notify({dist_do, Scenario, Start, End, Nodes, Comment}),
    Count = length(Nodes),
    [ amoc_controller:do(Node, Scenario, Start, End, Count, Id, Opts) ||
      {Id, Node} <- lists:zip(lists:seq(1, Count), Nodes) ].

add(Count) ->
    add(Count, nodes()).

add(Count, Nodes) ->
    amoc_event:notify({dist_add, Count}),
    [ amoc_controller:add(Node, Count) || Node <- Nodes ].

remove(Count, Opts) ->
    remove(Count, Opts, nodes()).

remove(Count, Opts, Nodes) ->
    amoc_event:notify({dist_remove, Count, Opts}),
    CountPerNode = ceil(Count / length(Nodes)),
    [ amoc_controller:remove(Node, CountPerNode, Opts) || Node <- Nodes ].

%% ------------------------------------------------------------------
%% Local functions
%% ------------------------------------------------------------------
start_nodes(Hosts, Path) ->
    [ amoc_slave:start(Host, Path) || Host <- Hosts ].

ceil(Number) ->
    case erlang:round(Number) of
        Lower when Lower<Number ->
            Lower+1;
        Greater ->
            Greater
    end.
