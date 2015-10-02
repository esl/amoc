%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).

-export_type([user_id/0]).

-type user_id() :: non_neg_integer().

-callback init() -> ok | {error, Reason :: term()}.
-callback start(user_id()) -> any().
