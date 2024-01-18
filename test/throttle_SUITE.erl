-module(throttle_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(DEFAULT_NO_PROCESSES, 10).
-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(RECV(Msg, Timeout), receive Msg -> ok after Timeout -> {error, not_received_yet} end).

all() ->
    [
     {group, api}
    ].

groups() ->
    [
     {api, [parallel],
      [
       start,
       rate_zero_is_not_accepted,
       low_rate_gets_remapped,
       low_interval_get_remapped,
       start_and_stop,
       change_rate,
       change_rate_gradually,
       send_and_wait,
       just_wait,
       wait_for_process_to_die_sends_a_kill,
       async_runner_dies_while_waiting_raises_exit,
       async_runner_dies_when_throttler_dies,
       run_with_interval_zero_limits_only_number_of_parallel_executions,
       pause_and_resume,
       get_state
     ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(amoc),
    amoc_cluster:set_master_node(node()),
    TelemetryEvents = [[amoc, throttle, Event] || Event <- [init, rate, request, execute, process]],
    telemetry_helpers:start(TelemetryEvents),
    Config.

end_per_suite(_) ->
    application:stop(amoc),
    telemetry_helpers:stop(),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%-----------------------------------------------------------------------------------
%% test cases
%%-----------------------------------------------------------------------------------

start(_) ->
    %% Starts successfully
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    %% If requested to start again, returns information accordingly
    ?assertMatch({ok, already_started},
                 amoc_throttle:start(?FUNCTION_NAME, 100)),
    ?assertMatch({error, wrong_reconfiguration},
                 amoc_throttle:start(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL + 1)),
    ?assertMatch({error, wrong_no_of_procs},
                 amoc_throttle:start(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL,
                                     ?DEFAULT_NO_PROCESSES + 1)).

rate_zero_is_not_accepted(_) ->
    ?assertMatch({error, invalid_throttle}, amoc_throttle:start(?FUNCTION_NAME, 0, 100, 1)).

low_rate_gets_remapped(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 2, 100, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 100,
                   delay_between_executions := 50},
                 State),
    assert_telemetry_event([amoc, throttle, process], error, ?FUNCTION_NAME, 2, 100).

low_interval_get_remapped(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 1, 1, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 1,
                   delay_between_executions := 10},
                 State),
    assert_telemetry_event([amoc, throttle, process], error, ?FUNCTION_NAME, 1, 1).

start_and_stop(_) ->
    %% Starts successfully
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    %% Stops successfully
    ?assertMatch(ok, amoc_throttle:stop(?FUNCTION_NAME)),
    WaitUntilFun = fun() -> get_number_of_workers(?FUNCTION_NAME) end,
    wait_helper:wait_until(WaitUntilFun, 0),
    %% And can start again successfully
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)).

change_rate(_) ->
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL)),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL)),
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL + 1)).

change_rate_gradually(_) ->
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, 100, 200, 1, 1, 1)),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    ?assertMatch(ok, amoc_throttle:change_rate_gradually(?FUNCTION_NAME, 10, 3000, 1, 100, 300)),
    %% We cannot change rate while a current gradual change is already running.
    ?assertMatch({error, cannot_change_rate},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, 50, 200, 1, 1, 1)),
    ?assertMatch({error, cannot_change_rate},
                 amoc_throttle:change_rate(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL + 1)).

send_and_wait(_) ->
    %% it failts if the throttle wasn't started yet
    ?assertMatch({error, no_throttle_process_registered},
                 amoc_throttle:send_and_wait(?FUNCTION_NAME, receive_this)),
    %% Start 100-per-10ms throttle with a single process
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 10, 1)),
    %% send_and_wait passes fine
    ?assertMatch(ok, amoc_throttle:send_and_wait(?FUNCTION_NAME, receive_this)),
    %% One message is received sufficiently fast
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, ?RECV(receive_this, 100)),
    %% If someone else fills the throttle heavily,
    %% it will take proportionally so long to execute for me
    fill_throttle(?FUNCTION_NAME, 100 * 10),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)).

just_wait(_) ->
    %% it failts if the throttle wasn't started yet
    ?assertMatch({error, no_throttle_process_registered},
                 amoc_throttle:wait(?FUNCTION_NAME)),
    %% Start 100-per-10ms throttle with a single process
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 10, 1)),
    %% wait passes fine
    ?assertMatch(ok, amoc_throttle:wait(?FUNCTION_NAME)),
    %% One message is received sufficiently fast
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, ?RECV(receive_this, 100)),
    %% If someone else fills the throttle heavily,
    %% it will take proportionally so long to execute for me
    fill_throttle(?FUNCTION_NAME, 100 * 10),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)).

wait_for_process_to_die_sends_a_kill(_) ->
    erlang:process_flag(trap_exit, true),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 10, 1)),
    amoc_throttle:run(?FUNCTION_NAME, fun() -> exit(?FUNCTION_NAME) end),
    ?assertMatch(ok, ?RECV({'EXIT', _, ?FUNCTION_NAME}, 100)).

async_runner_dies_while_waiting_raises_exit(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 1, 1, 1)),
    find_new_link_and_kill_it(self()),
    ?assertExit({throttle_wait_died, _, killed}, amoc_throttle:wait(?FUNCTION_NAME)).

async_runner_dies_when_throttler_dies(_) ->
    erlang:process_flag(trap_exit, true),
    {links, OriginalLinks} = erlang:process_info(self(), links),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 1, 60000, 1)),
    wait_until_one_throttle_worker(?FUNCTION_NAME),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    wait_until_one_async_runner(self(), OriginalLinks),
    amoc_throttle:stop(?FUNCTION_NAME),
    ?assertMatch(ok, ?RECV({'EXIT', _, {throttler_worker_died, _, _}}, 100)).

run_with_interval_zero_limits_only_number_of_parallel_executions(_) ->
    %% Start 10 actions at once in 10 processes
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 10, 0, 1)),
    %% If someone else fills the throttle heavily,
    %% it will take proportionally so long to execute for me
    fill_throttle(?FUNCTION_NAME, 100),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, ?RECV(receive_this, 200)).

pause_and_resume(_) ->
    %% Start 100-per-10ms throttle with a single process
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 10, 1)),
    %% send_and_wait passes fine
    ?assertMatch(ok, amoc_throttle:send_and_wait(?FUNCTION_NAME, receive_this)),
    %% pauses runs correctly
    ?assertMatch(ok, amoc_throttle:pause(?FUNCTION_NAME)),
    %% It is paused, so messages aren't received
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)),
    %% After resume the message is then received
    ?assertMatch(ok, amoc_throttle:resume(?FUNCTION_NAME)),
    ?assertMatch(ok, ?RECV(receive_this, 200)).

get_state(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 60000, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 60000,
                   delay_between_executions := 600},
                 State).


%% Helpers
assert_telemetry_event(Name, Measurement, Throttle, Rate, Interval) ->
    TelemetryEvents = telemetry_helpers:get_calls([amoc, throttle]),
    IsLowRateEventFn = fun({EventName, Measurements, Metadata}) ->
                               Name =:= EventName andalso
                               maps:is_key(Measurement, Measurements) andalso
                               Throttle =:= maps:get(name, Metadata, undefined) andalso
                               Rate =:= maps:get(rate, Metadata, undefined) andalso
                               Interval =:= maps:get(interval, Metadata, undefined)
                       end,
    ?assert(lists:any(IsLowRateEventFn, TelemetryEvents)).

get_throttle_workers(Name) ->
    pg:get_members(amoc_throttle, Name).

get_number_of_workers(Name) ->
    Processes = get_throttle_workers(Name),
    length(Processes).

get_state_of_one_process(Name) ->
    Processes = get_throttle_workers(Name),
    ?assertMatch([_ | _], Processes),
    [Process | _] = Processes,
    amoc_throttle_process:get_state(Process).

wait_until_one_throttle_worker(Name) ->
    GetWorkers = fun() -> get_throttle_workers(Name) end,
    Validator = fun(Res) -> 0 < length(Res) end,
    {ok, [Worker | _]} = wait_helper:wait_until(GetWorkers, ok, #{validator => Validator}),
    Worker.

fill_throttle(Name, Num) ->
    Parent = self(),
    spawn(fun() ->
                  [amoc_throttle:send(Name, self(), receive_this) || _ <- lists:seq(1, Num) ],
                  Parent ! continue
          end),
    receive
        continue -> ok
    end.

maybe_get_new_async_runners(Pid, OriginalLinks) ->
    {links, Links} = erlang:process_info(Pid, links),
    Links -- OriginalLinks.

wait_until_one_async_runner(Pid, OriginalLinks) ->
    GetLinksFun = fun() -> maybe_get_new_async_runners(Pid, OriginalLinks) end,
    Validator = fun(Res) -> 0 < length(Res) end,
    {ok, [AsyncRunner | _]} = wait_helper:wait_until(GetLinksFun, ok, #{validator => Validator}),
    AsyncRunner.

find_new_link_and_kill_it(Pid) ->
    erlang:process_flag(trap_exit, true),
    {links, OriginalLinks} = erlang:process_info(Pid, links),
    spawn(?MODULE, kill_async_runner, [Pid, OriginalLinks]).

kill_async_runner(Pid, OriginalLinks) ->
    GetLinksFun = fun() -> maybe_get_new_async_runners(Pid, OriginalLinks) end,
    Validator = fun(Res) -> 1 =:= length(Res) end,
    {ok, [AsyncRunner]} = wait_helper:wait_until(GetLinksFun, ok, #{validator => Validator}),
    exit(AsyncRunner, kill).

%% Helpers
amoc_do(Scenario) ->
    amoc_do(Scenario, 0, test_helpers:all_vars()).

amoc_do(Scenario, Count) ->
    amoc_do(Scenario, Count, test_helpers:all_vars()).

amoc_do(Scenario, Count, Config) ->
    amoc:do(Scenario, Count, Config).
