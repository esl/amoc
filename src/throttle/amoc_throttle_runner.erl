%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Asynchronous runner that always runs together with the requesting process.
%%
%% Knows how to distinguist between the different operations the caller needs.
-module(amoc_throttle_runner).

-export([throttle/2, run/1]).
-export([async_runner/3]).

-type action() :: wait | {pid(), term()} | fun(() -> any()).
-type runner() :: pid().
-export_type([runner/0]).

-spec run(runner()) -> reference().
run(RunnerPid) ->
    Ref = erlang:monitor(process, RunnerPid),
    RunnerPid ! '$scheduled',
    Ref.

-spec throttle(amoc_throttle:name(), action()) -> ok | {error, any()}.
throttle(Name, Action) ->
    case amoc_throttle_process:get_throttle_process(Name) of
        {ok, Throttler} ->
            RunnerPid = erlang:spawn_link(?MODULE, async_runner, [Name, self(), Action]),
            amoc_throttle_process:run(Throttler, RunnerPid),
            maybe_wait(Action, RunnerPid);
        Error ->
            Error
    end.

-spec maybe_wait(action(), runner()) -> ok.
maybe_wait(wait, RunnerPid) ->
    receive
        {'EXIT', RunnerPid, Reason} ->
            exit({throttle_wait_died, RunnerPid, Reason});
        '$scheduled' ->
            ok
    end;
maybe_wait(_, _) ->
    ok.

-spec async_runner(amoc_throttle:name(), pid(), term()) -> no_return().
async_runner(Name, Caller, Action) ->
    amoc_throttle_controller:raise_event_on_slave_node(Name, request),
    receive
        '$scheduled' ->
            execute(Caller, Action),
            amoc_throttle_controller:raise_event_on_slave_node(Name, execute),
            %% If Action failed, unlink won't be called and the caller will receive an exit signal
            erlang:unlink(Caller)
    end.

-spec execute(pid(), action()) -> term().
execute(Caller, wait) ->
    Caller ! '$scheduled';
execute(_Caller, Fun) when is_function(Fun, 0) ->
    Fun();
execute(_Caller, {Pid, Msg}) ->
    Pid ! Msg.
