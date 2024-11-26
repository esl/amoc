-module(throttle_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

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
       start_descriptive,
       start_interarrival,
       start_interarrival_zero,
       start_interarrival_infinity,
       start_rate_zero,
       start_rate_infinity,
       low_rate_does_not_get_remapped,
       low_interval_does_not_get_remapped,
       start_and_stop,
       change_rate,
       interval_equal_zero_limits_parallelism,
       change_rate_to_interval_zero_limits_parallelism,
       change_rate_triggers_parallelism,
       change_rate_gradually,
       change_interarrival_gradually,
       change_rate_gradually_verify_descriptions,
       just_wait,
       wait_for_process_to_die_sends_a_kill,
       async_runner_dies_while_waiting_raises_exit,
       async_runner_dies_when_throttler_dies,
       pause_and_resume_and_unlock,
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
                 amoc_throttle:start(?FUNCTION_NAME, 101)).

start_descriptive(_) ->
    %% Starts successfully
    Description = #{rate => 100, interval => 5000},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)).

start_interarrival(_) ->
    %% Starts successfully
    Description = #{interarrival => 50},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 60000, rate := 1200}, State).

start_interarrival_zero(_) ->
    %% Starts successfully
    Description = #{interarrival => 0},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 60000, rate := infinity}, State).

start_interarrival_infinity(_) ->
    %% Starts successfully
    Description = #{interarrival => infinity},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 60000, rate := 0}, State).

start_rate_zero(_) ->
    %% Starts successfully
    Description = #{rate => 0},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 60000, rate := 0}, State).

start_rate_infinity(_) ->
    %% Starts successfully
    Description = #{rate => infinity},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 60000, rate := infinity}, State).

low_rate_does_not_get_remapped(_) ->
    ?assertMatch({ok, started},
                 amoc_throttle:start(?FUNCTION_NAME,
                                     #{rate => 2, interval => 100})),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 100, rate := 2}, State).

low_interval_does_not_get_remapped(_) ->
    ?assertMatch({ok, started},
                 amoc_throttle:start(?FUNCTION_NAME,
                                     #{rate => 1, interval => 1})),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 1, rate := 1}, State).

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
                 amoc_throttle:change_rate(?FUNCTION_NAME, #{rate => 100})),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    E1 = #{rate => 100, interval => ?DEFAULT_INTERVAL},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, E1)),
    E2 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 1},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, E2)),
    E3 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 2},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, E3)),
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, 200)).

interval_equal_zero_limits_parallelism(_) ->
    E1 = #{rate => 36, interval => 0},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, E1)),
    #{pool_config := Config0} = get_throttle_info(?FUNCTION_NAME),
    ActiveWorkers = maps:filter(fun(_, #{status := S}) -> S =:= active end, Config0),
    ?assertEqual(1, map_size(ActiveWorkers)),
    ?assertMatch([#{max_n := 36, delay := 0}], maps:values(ActiveWorkers)).

change_rate_to_interval_zero_limits_parallelism(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    E1 = #{rate => 100, interval => 0},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, E1)),
    #{pool_config := Config0} = get_throttle_info(?FUNCTION_NAME),
    ?assertEqual(1, map_size(maps:filter(fun(_, #{status := S}) -> S =:= active end, Config0))).

change_rate_triggers_parallelism(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 1)),
    #{pool_config := Config0} = get_throttle_info(?FUNCTION_NAME),
    ?assertEqual(1, map_size(maps:filter(fun(_, #{status := S}) -> S =:= active end, Config0))),
    E1 = #{rate => 60000},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, E1)),
    #{pool_config := Config1} = get_throttle_info(?FUNCTION_NAME),
    ?assertNotEqual(1, map_size(maps:filter(fun(_, #{status := S}) -> S =:= active end, Config1))).

change_rate_gradually(_) ->
    C1 = #{from_rate => 100, to_rate => 200, interval => 1,
           step_interval => 1, step_count => 1},
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C1)),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    C2 = #{from_rate => 10, to_rate => 3000, interval => 1,
           step_interval => 100, step_count => 300},
    ?assertMatch(ok, amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C2)),
    %% We cannot change rate while a current gradual change is already running.
    C3 = #{from_rate => 50, to_rate => 200, interval => 1,
           step_interval => 1, step_count => 1},
    ?assertMatch({error, cannot_change_rate},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C3)),
    E1 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 1},
    ?assertMatch({error, cannot_change_rate}, amoc_throttle:change_rate(?FUNCTION_NAME, E1)).

change_interarrival_gradually(_) ->
    C1 = #{from_interarrival => 100, to_interarrival => 200},
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C1)),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    C2 = #{from_interarrival => 10, to_interarrival => 3000},
    ?assertMatch(ok, amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C2)),
    %% We cannot change rate while a current gradual change is already running.
    C3 = #{from_interarrival => 50, to_interarrival => 200},
    ?assertMatch({error, cannot_change_rate},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C3)),
    E1 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 1},
    ?assertMatch({error, cannot_change_rate}, amoc_throttle:change_rate(?FUNCTION_NAME, E1)).

%% Bad description also fails
change_rate_gradually_verify_descriptions(_) ->
    %% Condition 0
    D0 = #{from_rate => 1000, to_rate => 3000, duration => 30000},
    ?assertMatch(
       #{interval := 60000, step_size := 1, step_count := 2000, step_interval := 15},
       amoc_throttle_controller:verify_gradual_config(D0)),
    %% Condition 1
    D1 = #{from_rate => 3000, to_rate => 100, duration => 30000},
    ?assertMatch(
       #{interval := 60000, step_size := -1, step_count := 2900, step_interval := 10},
       amoc_throttle_controller:verify_gradual_config(D1)),
    %% Condition 2A
    D2A = #{from_rate => 0, to_rate => 3000, interval => 10000, step_size => 2},
    ?assertMatch(
       #{step_size := 2, step_count := 1500, step_interval := 2},
       amoc_throttle_controller:verify_gradual_config(D2A)),
    %% Condition 2B
    D2B = #{from_rate => 3000, to_rate => 0, interval => 10000, step_size => -2},
    ?assertMatch(
       #{step_size := -2, step_count := 1500, step_interval := 2},
       amoc_throttle_controller:verify_gradual_config(D2B)),
    %% Condition 3
    D3 = #{from_rate => 80, to_rate => 5000, interval => 1000, step_interval => 10},
    ?assertMatch(
       #{step_size := 1, step_count := 4920, step_interval := 10},
       amoc_throttle_controller:verify_gradual_config(D3)),
    %% Condition 4
    D4 = #{from_rate => 80, to_rate => 5000, interval => 15000, step_count => 90},
    ?assertMatch(
       #{step_size := 1, step_count := 90, step_interval := 54},
       amoc_throttle_controller:verify_gradual_config(D4)),
    %% Condition 5
    D5 = #{from_rate => 80, to_rate => 5000, interval => 15000,
           step_size => 5, step_interval => 50},
    ?assertMatch(
       #{step_size := 5, step_count := 984, step_interval := 50},
       amoc_throttle_controller:verify_gradual_config(D5)),
    %% Condition 6
    D6 = #{from_rate => 80, to_rate => 5000},
    ?assertMatch(
       #{interval := 60000, step_size := 1, step_count := 4920, step_interval := 1},
       amoc_throttle_controller:verify_gradual_config(D6)),
    %% Condition 7
    D7 = #{from_rate => 4000, to_rate => 1200},
    ?assertMatch(
       #{interval := 60000, step_size := -1, step_count := 2800, step_interval := 1},
       amoc_throttle_controller:verify_gradual_config(D7)),
    %% Error 1
    E1 = #{from_rate => 100, to_rate => 10, step_size => 1},
    ?assertMatch(
       {error, _},
       amoc_throttle_controller:verify_gradual_config(E1)).

just_wait(_) ->
    %% it fails if the throttle wasn't started yet
    ?assertMatch({error, no_throttle_process_registered},
                 amoc_throttle:wait(?FUNCTION_NAME)),
    %% Start 100-per-10ms throttle, that is, 600k per minute
    Description = #{rate => 100, interval => 10},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    %% wait passes fine
    ?assertMatch(ok, amoc_throttle:wait(?FUNCTION_NAME)),
    %% One message is received sufficiently fast
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, ?RECV(receive_this, 100)),
    %% If someone else fills the throttle heavily,
    %% it will take proportionally so long to execute for me
    %% TODO
    fill_throttle(?FUNCTION_NAME, 100 * 100),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)).

wait_for_process_to_die_sends_a_kill(_) ->
    erlang:process_flag(trap_exit, true),
    Description = #{rate => 100, interval => 10},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    amoc_throttle:run(?FUNCTION_NAME, fun() -> exit(?FUNCTION_NAME) end),
    ?assertMatch(ok, ?RECV({'EXIT', _, ?FUNCTION_NAME}, 100)).

async_runner_dies_while_waiting_raises_exit(_) ->
    Description = #{rate => 1, interval => 1},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    find_new_link_and_kill_it(self()),
    ?assertExit({throttle_wait_died, _, killed}, amoc_throttle:wait(?FUNCTION_NAME)).

async_runner_dies_when_throttler_dies(_) ->
    erlang:process_flag(trap_exit, true),
    {links, OriginalLinks} = erlang:process_info(self(), links),
    Description = #{rate => 1, interval => 60000},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    wait_until_one_throttle_worker(?FUNCTION_NAME),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    wait_until_one_async_runner(self(), OriginalLinks),
    amoc_throttle:stop(?FUNCTION_NAME),
    ?assertMatch(ok, ?RECV({'EXIT', _, {throttler_worker_died, _, _}}, 100)).

pause_and_resume_and_unlock(_) ->
    %% Start a 10-per-ms throttle
    Description = #{rate => 600000},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    ?assertMatch(ok, amoc_throttle:wait(?FUNCTION_NAME)),
    %% pauses runs correctly
    ?assertMatch(ok, amoc_throttle:pause(?FUNCTION_NAME)),
    %% It is paused, so messages aren't received
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)),
    %% After resume the message is then received
    ?assertMatch(ok, amoc_throttle:resume(?FUNCTION_NAME)),
    ?assertMatch(ok, ?RECV(receive_this, 200)),
    %% If unlocked, all messages are always received
    ?assertMatch(ok, amoc_throttle:unlock(?FUNCTION_NAME)),
    amoc_throttle:send(?FUNCTION_NAME, receive_this_too),
    ?assertMatch(ok, ?RECV(receive_this_too, 200)),
    %% From unlock it can resume
    ?assertMatch(ok, amoc_throttle:resume(?FUNCTION_NAME)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{rate := 600000}, State).

get_state(_) ->
    Config = #{rate => 100, interval => 60000},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Config)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{rate := 100, interval := 60000, active := true}, State).


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

get_throttle_info(Name) ->
    Info = amoc_throttle_controller:get_info(Name),
    ?assert(is_map(Info)),
    Info.

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
