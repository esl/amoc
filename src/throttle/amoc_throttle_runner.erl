%% @private
%% @see amoc_throttle
%% @doc Asynchronous runner that always runs together with the `amoc_user' process.
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_throttle_runner).

-export([spawn_run/2, spawn_send/3]).

-spec spawn_run(amoc_throttle:name(), amoc_throttle:action()) -> pid().
spawn_run(Name, Fun) ->
    amoc_throttle_controller:raise_event_on_slave_node(Name, request),
    erlang:spawn(fun() -> async_runner(Name, Fun) end).

-spec spawn_send(amoc_throttle:name(), pid(), term()) -> pid().
spawn_send(Name, Pid, Msg) ->
    amoc_throttle_controller:raise_event_on_slave_node(Name, request),
    erlang:spawn(fun() -> async_sender(Name, Pid, Msg) end).

-spec async_runner(amoc_throttle:name(), amoc_throttle:action()) -> no_return().
async_runner(Name, Fun) ->
    receive
        scheduled ->
            amoc_throttle_controller:raise_event_on_slave_node(Name, execute),
            Fun()
    end.

-spec async_sender(amoc_throttle:name(), pid(), term()) -> no_return().
async_sender(Name, Pid, Msg) ->
    receive
        scheduled ->
            amoc_throttle_controller:raise_event_on_slave_node(Name, execute),
            Pid ! Msg
    end.
