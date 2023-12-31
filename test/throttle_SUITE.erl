-module(throttle_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(DEFAULT_NO_PROCESSES, 10).
-define(DEFAULT_INTERVAL, 60000). %% one minute

all() ->
    [
     {group, api}
    ].

groups() ->
    [
     {api, [parallel],
      [
       start,
       rate_zero_gets_remapped_without_crashing,
       low_rate_gets_remapped,
       low_interval_get_remapped,
       start_and_stop,
       change_rate,
       change_rate_gradually,
       send_and_wait,
       run_with_interval_zero_runs_very_fast,
       pause_and_resume,
       get_state
     ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(amoc),
    amoc_cluster:set_master_node(node()),
    Config.

end_per_suite(_) ->
    application:stop(amoc),
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

rate_zero_gets_remapped_without_crashing(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 0, 100, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 100,
                   delay_between_executions := 100},
                 State).

low_rate_gets_remapped(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 2, 100, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 100,
                   delay_between_executions := 50},
                 State).

low_interval_get_remapped(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 1, 1, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 1,
                   delay_between_executions := 10},
                 State).

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
    ?assertMatch({ok, started},
                 amoc_throttle:start(?FUNCTION_NAME, 100)),
    ?assertMatch(ok,
                 amoc_throttle:change_rate(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL)),
    ?assertMatch({ok, 99},
                 amoc_throttle:change_rate(?FUNCTION_NAME, 100, ?DEFAULT_INTERVAL + 1)).

change_rate_gradually(_) ->
    ?assertMatch({error, {no_throttle_by_name, ?FUNCTION_NAME}},
                 amoc_throttle:change_rate_gradually(
                   ?FUNCTION_NAME, 100, 200, 1, 1, 1)),
    ?assertMatch({ok, started},
                 amoc_throttle:start(?FUNCTION_NAME, 100)),
    ?assertMatch(ok,
                 amoc_throttle:change_rate_gradually(
                   ?FUNCTION_NAME, 50, 200, 1, 1, 1)).

send_and_wait(_) ->
    %% it failts if the throttle wasn't started yet
    ?assertMatch({error, {no_throttle_process_registered, ?FUNCTION_NAME}},
                 amoc_throttle:send_and_wait(?FUNCTION_NAME, receive_this)),
    %% Start 100-per-10ms throttle with a single process
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 10, 1)),
    %% send_and_wait passes fine
    ?assertMatch(ok, amoc_throttle:send_and_wait(?FUNCTION_NAME, receive_this)),
    %% One message is received sufficiently fast
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, receive_msg_in_timeout(receive_this, 100)),
    %% If someone else fills the throttle heavily,
    %% it will take proportionally so long to execute for me
    fill_throttle(?FUNCTION_NAME, 100 * 10),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, receive_msg_in_timeout(receive_this, 1000)).

run_with_interval_zero_runs_very_fast(_) ->
    %% Start 10 actions at once in 10 processes
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 10, 0, 1)),
    %% If someone else fills the throttle heavily,
    %% it will take proportionally so long to execute for me
    fill_throttle(?FUNCTION_NAME, 100),
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch(ok, receive_msg_in_timeout(receive_this, 1000)).

pause_and_resume(_) ->
    %% Start 100-per-10ms throttle with a single process
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 10, 1)),
    %% send_and_wait passes fine
    ?assertMatch(ok, amoc_throttle:send_and_wait(?FUNCTION_NAME, receive_this)),
    %% pauses runs correctly
    ?assertMatch(ok, amoc_throttle:pause(?FUNCTION_NAME)),
    %% It is paused, so messages aren't received
    amoc_throttle:send(?FUNCTION_NAME, receive_this),
    ?assertMatch({error, not_received_yet}, receive_msg_in_timeout(receive_this, 1000)),
    %% After resume the message is then received
    ?assertMatch(ok, amoc_throttle:resume(?FUNCTION_NAME)),
    ?assertMatch(ok, receive_msg_in_timeout(receive_this, 1000)).

get_state(_) ->
    ?assertMatch({ok, started}, amoc_throttle:start(?FUNCTION_NAME, 100, 60000, 1)),
    State = get_state_of_one_process(?FUNCTION_NAME),
    ?assertMatch(#{name := ?FUNCTION_NAME,
                   interval := 60000,
                   delay_between_executions := 600},
                 State).


%% Helpers
get_number_of_workers(Name) ->
    Processes = pg:get_members(amoc_throttle, Name),
    length(Processes).

get_state_of_one_process(Name) ->
    Processes = pg:get_members(amoc_throttle, Name),
    ?assertMatch([_ | _], Processes),
    [Process | _] = Processes,
    amoc_throttle_process:get_state(Process).

fill_throttle(Name, Num) ->
    Parent = self(),
    spawn(fun() ->
                  [amoc_throttle:send(Name, self(), receive_this) || _ <- lists:seq(1, Num) ],
                  Parent ! continue
          end),
    receive
        continue -> ok
    end.

receive_msg_in_timeout(Msg, Timeout) ->
    receive
        Msg ->
            ok
    after Timeout ->
              {error, not_received_yet}
    end.

%% Helpers
amoc_do(Scenario) ->
    amoc_do(Scenario, 0, test_helpers:all_vars()).

amoc_do(Scenario, Count) ->
    amoc_do(Scenario, Count, test_helpers:all_vars()).

amoc_do(Scenario, Count, Config) ->
    amoc:do(Scenario, Count, Config).
