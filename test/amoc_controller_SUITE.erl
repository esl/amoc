-module(amoc_controller_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([scenario_is_terminated_when_stopped/1,
         scenario_is_not_terminated_when_not_stopped/1,
         checking_will_not_start_when_any_callback_is_not_exported/1,
         checking_callback_exceptions_are_caught/1
        ]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     scenario_is_terminated_when_stopped,
     scenario_is_not_terminated_when_not_stopped,
     checking_will_not_start_when_any_callback_is_not_exported,
     checking_callback_exceptions_are_caught
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(amoc),
    ok = application:set_env(amoc, scenario_checking_interval, 0),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

scenario_is_terminated_when_stopped(_Config) ->
    %% GIVEN
    S = self(),
    StopReason = make_ref(),
    meck:new(test_scenario, []),
    meck:expect(test_scenario, continue, fun() -> {stop, StopReason} end),
    meck:expect(test_scenario, terminate, fun(Reason) -> S ! Reason end),

    %% WHEN
    amoc_controller:start_scenario_checking(test_scenario),

    %% THEN
    receive
        Reason ->
            ?assertMatch(StopReason, Reason)
    end.

scenario_is_not_terminated_when_not_stopped(_Config) ->
    %% GIVEN
    S = self(),
    Ref = make_ref(),
    meck:new(test_scenario, []),
    meck:expect(test_scenario, continue, fun() -> continue end),
    meck:expect(test_scenario, terminate, fun(_) -> S ! Ref end),

    %% WHEN
    amoc_controller:start_scenario_checking(test_scenario),

    %% THEN
    receive
        RecvRef ->
            ?assertNotMatch(Ref, RecvRef)
    after
        50 ->
            true
    end.

checking_will_not_start_when_any_callback_is_not_exported(Config) ->
    %% GIVEN
    meck:new(test_scenario, []),
    meck:expect(test_scenario, continue, fun() -> {stop, test} end),

    %% WHEN
    Res = amoc_controller:start_scenario_checking(test_scenario),

    %% THEN
    ?assertMatch(Res, skip).

checking_callback_exceptions_are_caught(_Config) ->
    %% GIVEN
    S = self(),
    meck:new(test_scenario, []),
    meck:expect(test_scenario, continue, fun() -> error(continue) end),
    meck:expect(test_scenario, terminate, fun(Reason) ->
                                                  S ! Reason,
                                                  error(terminate)
                                          end),

    %% WHEN
    amoc_controller:start_scenario_checking(test_scenario),

    %% THEN
    timer:sleep(100),
    receive
        Reason ->
            ?assertMatch({error, continue}, Reason)
    after
        50 ->
            error(recv_assert_match_timeout)
    end,
    Apps = application:which_applications(),
    ?assertNot(proplists:is_defined(amoc, Apps)).
