%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc).

-export([do/3,
         add/1,
         remove/2]).

-export_type([scenario/0]).
-type scenario() :: module().

%% ------------------------------------------------------------------
%% API for the local scenario execution, use amoc_dist module to run
%% scenarios in a distributed environment
%% ------------------------------------------------------------------
-spec do(scenario(), non_neg_integer(), amoc_config_scenario:config()) ->
    ok | {error, term()}.
do(Scenario, Count, Settings) ->
    case amoc_cluster:set_master_node(node()) of
        ok ->
            %% amoc_controller:start_scenario/2 will fail,
            %% if amoc is running in a distributed mode
            case {amoc_controller:start_scenario(Scenario, Settings), Count} of
                {ok, 0} -> ok;
                {ok, Count} -> amoc_controller:add_users(1, Count);
                Error -> Error
            end;
        Error -> Error
    end.

-spec add(pos_integer()) -> ok | {error, any()}.
add(Count) when is_integer(Count), Count > 0 ->
    Node = node(),
    case amoc_cluster:get_master_node() of
        undefined -> {error, master_node_is_not_set};
        Node ->
            %% amoc_controller:add_users/2 will fail,
            %% if amoc is running in a distributed mode
            {running, _, _, LastUserId} = amoc_controller:get_status(),
            amoc_controller:add_users(LastUserId + 1, LastUserId + Count);
        _ -> {error, node_is_clustered}
    end.

-spec remove(pos_integer(), boolean()) -> {ok, non_neg_integer()} | {error, any()}.
remove(Count, ForceRemove) when is_integer(Count), Count > 0 ->
    Node = node(),
    case amoc_cluster:get_master_node() of
        undefined -> {error, master_node_is_not_set};
        Node ->
            %% amoc_controller:remove_users/2 will fail,
            %% if amoc is running in a distributed mode
            amoc_controller:remove_users(Count, ForceRemove);
        _ -> {error, node_is_clustered}
    end.