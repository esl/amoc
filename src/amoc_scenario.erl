%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).

-export_type([user_id/0, state/0]).

-type user_id() :: non_neg_integer().
-type state() :: any().

-callback init() -> {ok, state()} | ok | {error, Reason :: term()}.
-callback start(user_id(), state()) -> any().
-callback start(user_id()) -> any().
-callback continue() -> continue | {stop, Reason :: term()}.
-callback terminate(Reason :: term()) -> any().
-callback next_user_batch(BatchIndex :: non_neg_integer(),
                          PrevUserCount :: non_neg_integer()) ->
    amoc_controller:user_batch_strategy().

%% either start/1 or start/2 must be exported from the behaviour module
-optional_callbacks([start/1, start/2]).
-optional_callbacks([continue/0, terminate/1, next_user_batch/2]).
