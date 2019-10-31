%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc).

-export_type([
              scenario/0,
              do_opts/0,
              remove_opts/0
             ]).

-type scenario() :: module().

-type do_opt() :: {nodes, [node()]} | {comment, string()} | {repeat, timeout()}
                  | {interarrival, timeout()}.
-type do_opts() :: [do_opt()].

-type remove_opt() :: {force, boolean()}.
-type remove_opts() :: [remove_opt()].
