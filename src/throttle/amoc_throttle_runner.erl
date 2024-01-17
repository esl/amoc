%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Asynchronous runner that always runs together with the `amoc_user' process.
-module(amoc_throttle_runner).

-export([spawn/2, run/1]).
-export([async_runner/2]).

-type runner() :: pid().
-export_type([runner/0]).

-spec spawn(amoc_throttle:name(), amoc_throttle:action()) -> pid().
spawn(Name, Fun) ->
    amoc_throttle_controller:raise_event_on_slave_node(Name, request),
    erlang:spawn(?MODULE, async_runner, [Name, Fun]).

-spec async_runner(amoc_throttle:name(), amoc_throttle:action()) -> no_return().
async_runner(Name, Fun) ->
    receive
        scheduled ->
            amoc_throttle_controller:raise_event_on_slave_node(Name, execute),
            Fun()
    end.

-spec run(runner()) -> reference().
run(RunnerPid) ->
    Ref = erlang:monitor(process, RunnerPid),
    RunnerPid ! scheduled,
    Ref.
