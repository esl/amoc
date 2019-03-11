-module(amoc_controller_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([
         scenario_is_terminated_when_stopped/1,
         scenario_is_not_terminated_when_not_stopped/1,
         checking_will_not_start_when_any_callback_is_not_exported/1,
         checking_callback_exceptions_are_caught/1
        ]).

%%--------------------------------------------------------------------
%% Supporting macros
%%--------------------------------------------------------------------

-define(recvAssertMatch(P), fun(P) ->
                                    receive
                                        V ->
                                            ?assertMatch(V, P)
                                    after 100 ->
                                            error("Receive assert match timeout,"
                                                  " pattern=~p", [P])
                                    end
                            end).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, scenario_checking}
    ].

groups() ->
    [
     {scenario_checking, [shuffle],
      [
       scenario_is_terminated_when_stopped,
       scenario_is_not_terminated_when_not_stopped,
       checking_will_not_start_when_any_callback_is_not_exported,
       checking_callback_exceptions_are_caught
      ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(amoc),
    %% Mock test_scenario
    meck:new(test_scenario, [non_strict, no_link]),
    Config.

end_per_suite(Config) ->
    meck:unload(test_scenario),
    Config.

init_per_group(scenario_checking, Config) ->
    ok = application:set_env(amoc, scenario_checking_interval, 0),
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(checking_callback_exceptions_are_caught, Config) ->
    meck:new(application, [unstick, passthrough, no_link]),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(checking_will_not_start_when_any_callback_is_not_exported,
                 Config) ->
    meck:unload(test_scenario_no_exports),
    Config;
end_per_testcase(checking_callback_exceptions_are_caught, Config) ->
    meck:unload(application),
    Config;
end_per_testcase(_Case, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% GROUP scenario_checking
%%--------------------------------------------------------------------

scenario_is_terminated_when_stopped(_Config) ->
    %% GIVEN
    S = self(),
    StopReason = make_ref(),
    meck:expect(test_scenario, continue, fun() -> {stop, StopReason} end),
    meck:expect(test_scenario, terminate, fun(Reason) -> S ! Reason end),

    %% WHEN
    amoc_controller:start_scenario_checking(test_scenario),

    %% THEN
    ?recvAssertMatch(StopReason).

scenario_is_not_terminated_when_not_stopped(_Config) ->
    %% GIVEN
    S = self(),
    Ref = make_ref(),
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

checking_will_not_start_when_any_callback_is_not_exported(_Config) ->
    %% GIVEN
    meck:new(test_scenario_no_exports, [non_strict]),

    %% WHEN
    Res = amoc_controller:start_scenario_checking(test_scenario_no_exports),

    %% THEN
    ?assertMatch(Res, skip).

checking_callback_exceptions_are_caught(_Config) ->
    %% GIVEN
    S = self(),
    meck:expect(test_scenario, continue, fun() -> error(continue) end),
    meck:expect(test_scenario, terminate, fun(Reason) ->
                                                  S ! Reason,
                                                  error(terminate)
                                          end),
    meck:expect(application, stop, fun(amoc) ->
                                           S ! amoc_stopped;
                                      (App) ->
                                           meck:passthrough(App)
                                   end),

    %% WHEN
    amoc_controller:start_scenario_checking(test_scenario),

    %% THEN
    timer:sleep(100),
    [?recvAssertMatch(P) || P <- [{error, continue}, amoc_stopped]].
