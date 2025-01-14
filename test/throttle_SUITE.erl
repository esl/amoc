-module(throttle_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(RECV(Msg, Timeout), receive Msg -> ok after Timeout -> {error, not_received_yet} end).

all() ->
    [
     {group, api},
     {group, properties}
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
       start_interval_zero,
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
       pause_and_resume,
       get_state
     ]},
     {properties, [],
      [
       change_rate_gradually_verify_descriptions_properties,
       % Note that the smallest delay possible for a process is 1ms (receive operations),
       % hence if we give for example 10 workers 1ms delays, we get 600_000 ticks per minute.
       % and if we give for example 48 workers 1ms delays, we get 2_880_000 ticks per minute.
       % That means, that is realistically the maximum rate we could possibly manage
       % with a static pool of such number of workers.
       pool_config_is_precise_for_rates_1,
       pool_config_is_precise_for_rates_2,
       pool_config_is_precise_for_rates_3,
       pool_config_is_precise_for_rates_4,
       pool_config_is_precise_for_rates_5,
       pool_config_is_precise_for_rates_6,
       pool_config_is_precise_for_rates_7,
       pool_config_is_precise_for_rates_8,
       pool_config_is_precise_for_rates_9,
       pool_config_is_precise_for_rates_10
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

init_per_group(properties, Config) ->
    meck:new(amoc_throttle_config, [passthrough, non_strict, no_link]),
    meck:expect(amoc_throttle_config, no_of_processes, [], 100),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(properties, _Config) ->
    meck:unload(amoc_throttle_config);
end_per_group(_, _Config) ->
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

start_interval_zero(_) ->
    %% Starts successfully
    Description = #{rate => 60, interval => 0},
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, Description)),
    State = get_throttle_info(?FUNCTION_NAME),
    ?assertMatch(#{interval := 0, rate := 60}, State).

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
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, 101)),
    R1 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 1},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, R1)),
    R2 = #{rate => 10000, interval => 100},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, R2)),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, ?RECV(receive_this, 100)),
    R3 = #{rate => 0},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, R3)),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)),
    R4 = #{rate => infinity},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, R4)),
    ?assertMatch(ok, ?RECV(receive_this, 100)),
    I1 = #{interarrival => 1},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, I1)),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, ?RECV(receive_this, 100)),
    I2 = #{interarrival => 30000},
    ?assertMatch(ok, amoc_throttle:change_rate(?FUNCTION_NAME, I2)),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, ?RECV(receive_this, 200)).

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
    C1 = #{throttle => #{from_rate => 100, to_rate => 200, interval => 1},
           plan => #{step_interval => 1, step_count => 1}},
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C1)),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    C2 = #{throttle => #{from_rate => 10, to_rate => 3000, interval => 1},
           plan => #{step_interval => 100, step_count => 300}},
    ?assertMatch(ok, amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C2)),
    %% We cannot change rate while a current gradual change is already running.
    C3 = #{throttle => #{from_rate => 50, to_rate => 200, interval => 1},
           plan => #{step_interval => 1, step_count => 1}},
    ?assertMatch({error, cannot_change_rate},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C3)),
    E1 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 1},
    ?assertMatch({error, cannot_change_rate}, amoc_throttle:change_rate(?FUNCTION_NAME, E1)).

change_interarrival_gradually(_) ->
    C1 = #{throttle => #{from_interarrival => 100, to_interarrival => 200},
           plan => #{step_interval => 1, step_count => 1}},
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C1)),
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100)),
    C2 = #{throttle => #{from_interarrival => 10, to_interarrival => 3000},
           plan => #{step_interval => 10, step_count => 1000}},
    ?assertMatch(ok, amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C2)),
    %% We cannot change rate while a current gradual change is already running.
    C3 = #{throttle => #{from_interarrival => 50, to_interarrival => 200},
           plan => #{step_interval => 1, step_count => 1}},
    ?assertMatch({error, cannot_change_rate},
                 amoc_throttle:change_rate_gradually(?FUNCTION_NAME, C3)),
    E1 = #{rate => 100, interval => ?DEFAULT_INTERVAL + 1},
    ?assertMatch({error, cannot_change_rate}, amoc_throttle:change_rate(?FUNCTION_NAME, E1)).

change_rate_gradually_verify_descriptions(_) ->
    %%% Using step_interval and step_count
    %% Condition 1: increment, explicit interval
    D1 = #{throttle => #{from_rate => 80, to_rate => 5000, interval => 15000},
           plan => #{step_interval => 50, step_count => 984}},
    R1 = amoc_throttle_config:verify_gradual_config(D1),
    ?assertMatch(
       #{rates := Rates, interval := 15000, step_interval := 50} when 985 =:= length(Rates), R1),
    ?assertEqual(lists:sort(maps:get(rates, R1)), maps:get(rates, R1)),
    %% Condition 2: decrement, explicit interval
    D2 = #{throttle => #{from_rate => 5000, to_rate => 80, interval => 1000},
           plan => #{step_interval => 10, step_count => 4920}},
    R2 = amoc_throttle_config:verify_gradual_config(D2),
    ?assertMatch(
       #{rates := Rates, interval := 1000, step_interval := 10} when 4921 =:= length(Rates), R2),
    ?assertEqual(lists:reverse(lists:sort(maps:get(rates, R2))), maps:get(rates, R2)),
    %% Condition 3: increment, default interval
    D3 = #{throttle => #{from_rate => 1200, to_rate => 4000},
           plan => #{step_interval => 10, step_count => 100}},
    R3 = amoc_throttle_config:verify_gradual_config(D3),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 10} when 101 =:= length(Rates), R3),
    ?assertEqual(lists:sort(maps:get(rates, R3)), maps:get(rates, R3)),
    %% Condition 4: decrement, default interval
    D4 = #{throttle => #{from_rate => 4000, to_rate => 1200},
           plan => #{step_interval => 10, step_count => 100}},
    R4 = amoc_throttle_config:verify_gradual_config(D4),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 10} when 101 =:= length(Rates), R4),
    ?assertEqual(lists:reverse(lists:sort(maps:get(rates, R4))), maps:get(rates, R4)),
    %% Condition 5: increment, interarrival
    D5 = #{throttle => #{from_interarrival => 1000, to_interarrival => 100},
           plan => #{step_interval => 10, step_count => 100}},
    R5 = amoc_throttle_config:verify_gradual_config(D5),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 10} when 101 =:= length(Rates), R5),
    ?assertEqual(lists:sort(maps:get(rates, R5)), maps:get(rates, R5)),
    %% Condition 6: decrement, interarrival
    D6 = #{throttle => #{from_interarrival => 100, to_interarrival => 1000},
           plan => #{step_interval => 10, step_count => 100}},
    R6 = amoc_throttle_config:verify_gradual_config(D6),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 10} when 101 =:= length(Rates), R6),
    ?assertEqual(lists:reverse(lists:sort(maps:get(rates, R6))), maps:get(rates, R6)),

    %%% Using step_interval and step_count
    %% Condition 7: increment, explicit interval
    D7 = #{throttle => #{from_rate => 80, to_rate => 5000, interval => 15000},
           plan => #{duration => timer:minutes(10)}},
    R7 = amoc_throttle_config:verify_gradual_config(D7),
    ?assertMatch(
       #{rates := Rates, interval := 15000, step_interval := 100} when 6001 =:= length(Rates), R7),
    ?assertEqual(lists:sort(maps:get(rates, R7)), maps:get(rates, R7)),
    %% Condition 8: decrement, explicit interval
    D8 = #{throttle => #{from_rate => 5000, to_rate => 80, interval => 1000},
           plan => #{duration => timer:minutes(10)}},
    R8 = amoc_throttle_config:verify_gradual_config(D8),
    ?assertMatch(
       #{rates := Rates, interval := 1000, step_interval := 100} when 6001 =:= length(Rates), R8),
    ?assertEqual(lists:reverse(lists:sort(maps:get(rates, R8))), maps:get(rates, R8)),
    %% Condition 9: increment, default interval
    D9 = #{throttle => #{from_rate => 1200, to_rate => 4000},
           plan => #{duration => timer:minutes(30)}},
    R9 = amoc_throttle_config:verify_gradual_config(D9),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 100} when 18001 =:= length(Rates), R9),
    ?assertEqual(lists:sort(maps:get(rates, R9)), maps:get(rates, R9)),
    %% Condition 10: decrement, default interval
    D10 = #{throttle => #{from_rate => 4000, to_rate => 1200},
           plan => #{duration => timer:minutes(10)}},
    R10 = amoc_throttle_config:verify_gradual_config(D10),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 100} when 6001 =:= length(Rates), R10),
    ?assertEqual(lists:reverse(lists:sort(maps:get(rates, R10))), maps:get(rates, R10)),
    %% Condition 11: increment, interarrival
    D11 = #{throttle => #{from_interarrival => 1000, to_interarrival => 100},
           plan => #{duration => timer:minutes(10)}},
    R11 = amoc_throttle_config:verify_gradual_config(D11),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 100} when 6001 =:= length(Rates), R11),
    ?assertEqual(lists:sort(maps:get(rates, R11)), maps:get(rates, R11)),
    %% Condition 12: decrement, interarrival
    D12 = #{throttle => #{from_interarrival => 100, to_interarrival => 1000},
           plan => #{duration => timer:minutes(10)}},
    R12 = amoc_throttle_config:verify_gradual_config(D12),
    ?assertMatch(
       #{rates := Rates, interval := 60000, step_interval := 100} when 6001 =:= length(Rates), R12),
    ?assertEqual(lists:reverse(lists:sort(maps:get(rates, R12))), maps:get(rates, R12)),

    %% Error
    E1 = #{throttle => #{from_rate => 100, to_rate => 10},
           plan => #{}},
    ?assertMatch(
       {error, _},
       amoc_throttle_config:verify_gradual_config(E1)).

change_rate_gradually_verify_descriptions_properties(_) ->
    Fun = fun(From, To, Interval, StepInterval, StepCount) ->
              D1 = #{throttle => #{from_rate => From, to_rate => To, interval => Interval},
                     plan => #{step_interval => StepInterval, step_count => StepCount}},
              R1 = amoc_throttle_config:verify_gradual_config(D1),
              ?assertMatch(#{rates := Rates,
                             interval := Interval,
                             step_interval := StepInterval}
                             when 1 + StepCount =:= length(Rates), R1),
              Rates = maps:get(rates, R1),
              SortedRates = lists:sort(Rates),
              From =:= lists:nth(1, Rates) andalso
              To =:= lists:last(Rates) andalso
              (From =< To andalso SortedRates =:= Rates
               orelse From > To andalso lists:reverse(SortedRates) =:= Rates)
    end,
    Prop = ?FORALL({From, To, Interval, StepInterval, StepCount},
                   {integer(1, 1 bsl 61),
                    integer(1, 1 bsl 61),
                    integer(1, 1 bsl 24),
                    integer(1, 1 bsl 8),
                    integer(1, 1 bsl 24)},
                   Fun(From, To, Interval, StepInterval, StepCount)),
    run_prop(?FUNCTION_NAME, Prop, 1 bsl 16, 1).

pool_config_is_precise_for_rates_1(_) ->
    pool_config_property_tests(lists:seq(1, 100_000), timer:minutes(1)).

pool_config_is_precise_for_rates_2(_) ->
    pool_config_property_tests(lists:seq(100_000, 200_000), timer:minutes(1)).

pool_config_is_precise_for_rates_3(_) ->
    pool_config_property_tests(lists:seq(200_000, 300_000), timer:minutes(1)).

pool_config_is_precise_for_rates_4(_) ->
    pool_config_property_tests(lists:seq(300_000, 400_000), timer:minutes(1)).

pool_config_is_precise_for_rates_5(_) ->
    pool_config_property_tests(lists:seq(400_000, 500_000), timer:minutes(1)).

pool_config_is_precise_for_rates_6(_) ->
    pool_config_property_tests(lists:seq(500_000, 600_000), timer:minutes(1)).

pool_config_is_precise_for_rates_7(_) ->
    pool_config_property_tests(lists:seq(600_000, 700_000), timer:minutes(1)).

pool_config_is_precise_for_rates_8(_) ->
    pool_config_property_tests(lists:seq(700_000, 800_000), timer:minutes(1)).

pool_config_is_precise_for_rates_9(_) ->
    pool_config_property_tests(lists:seq(800_000, 900_000), timer:minutes(1)).

pool_config_is_precise_for_rates_10(_) ->
    pool_config_property_tests(lists:seq(900_000, 1_000_000), timer:minutes(1)).

% TODO: introduce dynamically sized pools in order to manage higher rates.
pool_config_is_precise_for_high_rates(_) ->
    pool_config_property_tests(lists:seq(1 bsl 24, 1 bsl 32), timer:minutes(1)).

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

pause_and_resume(_) ->
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

pool_config_property_tests(RateGen, IntervalGen) ->
    Fun = fun(Rate, Interval) ->
              R1 = amoc_throttle_config:pool_config(Rate, Interval),
              accumulated_is_requested(Rate, Interval, R1)
    end,
    [ Fun(Rate, IntervalGen) || Rate <- RateGen].

accumulated_is_requested(Rate, Interval, Res) ->
    Fold = fun(_N, #{status := active, delay := D}, Acc) ->
                   Acc + (1 / D);
              (_, _, Acc) ->
                   Acc
           end,
    NumberOfActionsPerMs = maps:fold(Fold, +0.0, Res),
    Expected = Rate / Interval,
    %% This is checking that the difference is less than 1% by multipliting
    %% by a 100 and then checking it is smaller.
    Error = (abs(NumberOfActionsPerMs - Expected) * 100),
    Expected >= Error
    orelse throw(#{throttle => #{rate => Rate, interval => Interval},
                   expected_rate_per_minute => Expected,
                   returned_aggregated_rate_per_minute => NumberOfActionsPerMs,
                   error_percentage => Error/Expected,
                   config => filter_only_actives(Res)}).

filter_only_actives(Res) ->
    maps:filter(fun(_, #{status := Status}) -> Status =:= active end, Res).

run_prop(PropName, Property, NumTests, Workers) ->
    Opts = [noshrink, {start_size, 1}, {numtests, NumTests}, {numworkers, Workers}],
    Res = proper:counterexample(proper:conjunction([{PropName, Property}]), Opts),
    ?assertEqual(true, Res).

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
