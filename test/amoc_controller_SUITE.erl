-module(amoc_controller_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([
         scenario_is_terminated_when_stopped/1,
         scenario_is_not_terminated_when_not_stopped/1,
         checking_will_not_start_when_any_callback_is_not_exported/1,
         checking_callback_exceptions_are_caught/1,
         user_batches_are_added_according_to_strategy/1,
         user_batches_number_can_be_limited/1,
         user_batch_callback_receives_valid_parameters/1,
         user_batches_respect_user_interarrival/1,
         user_batches_will_not_be_added_when_callback_is_not_exported/1
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
     {group, scenario_checking},
     {group, user_batches}
    ].

groups() ->
    [
     {scenario_checking, [shuffle],
      [
       scenario_is_terminated_when_stopped,
       scenario_is_not_terminated_when_not_stopped,
       checking_will_not_start_when_any_callback_is_not_exported,
       checking_callback_exceptions_are_caught
      ]},
     {user_batches, [shuffle],
      [
       user_batches_are_added_according_to_strategy,
       user_batches_number_can_be_limited,
       user_batch_callback_receives_valid_parameters,
       user_batches_respect_user_interarrival,
       user_batches_will_not_be_added_when_callback_is_not_exported
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
    Config;
init_per_group(user_batches, Config) ->
    ok = application:set_env(amoc, add_batch_interval, 0),
    ok = application:set_env(amoc, interarrival, 0),
    %% Mock supervisor:start_child/2 in order to change the behavior of
    %% amoc_controller:start_user/4. Now a user is no longer a process but only
    %% an entry in an ETS table. It simplifies testing substantially.
    meck:new(supervisor, [unstick, passthrough, no_link]),
    meck:expect(supervisor, start_child,
                fun(amoc_users_sup, [_, Id, _]) ->
                        ets:insert(amoc_users, {Id, self()});
                   (Pid, Args) ->
                        meck:passthrough([Pid, Args])
                end),
    Config.

end_per_group(user_batches, Config) ->
    meck:unload(supervisor),
    Config;
end_per_group(_Group, Config) ->
    Config.

init_per_testcase(checking_callback_exceptions_are_caught, Config) ->
    meck:new(application, [unstick, passthrough, no_link]),
    Config;
init_per_testcase(Case, Config) when
      Case == user_batches_are_added_according_to_strategy orelse
      Case == user_batches_number_can_be_limited orelse
      Case == user_batch_callback_receives_valid_parameters orelse
      Case == user_batches_respect_user_interarrival ->
    setup_amoc_controller(),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(Case, Config) when
      Case == checking_will_not_start_when_any_callback_is_not_exported orelse
      Case == user_batches_will_not_be_added_when_callback_is_not_exported ->
    meck:unload(test_scenario_no_exports),
    Config;
end_per_testcase(checking_callback_exceptions_are_caught, Config) ->
    meck:unload(application),
    Config;
end_per_testcase(Case, Config) when
      Case == user_batches_are_added_according_to_strategy orelse
      Case == user_batches_number_can_be_limited orelse
      Case == user_batch_callback_receives_valid_parameters orelse
      Case == user_batches_respect_user_interarrival ->
    reset_amoc_controller(),
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

%%--------------------------------------------------------------------
%% GROUP user_batches
%%--------------------------------------------------------------------

user_batches_are_added_according_to_strategy(_Config) ->
    %% GIVEN
    Node1Users = 10,
    Node2Users = 20,
    TotalUsers = Node1Users + Node2Users,
    BatchStrategy = [{node(), Node1Users, 0},
                     {node(), Node2Users, 0}],
    BatchCount = 1,
    meck:expect(test_scenario, next_user_batch, fun(_, _) -> BatchStrategy end),

    %% WHEN
    amoc_controller:add_batches(BatchCount, test_scenario),

    %% THEN
    timer:sleep(100),
    ?assertMatch(TotalUsers,
                 proplists:get_value(count, amoc_controller:users())).

user_batches_number_can_be_limited(_Config) ->
    %% GIVEN
    UserCount = 1,
    BatchStrategy = [{node(), UserCount, 0}],
    BatchCount = 10,
    TotalUsers = UserCount * BatchCount,
    meck:expect(test_scenario, next_user_batch, fun(_, _) -> BatchStrategy end),

    %% WHEN
    amoc_controller:add_batches(BatchCount, test_scenario),

    %% THEN
    timer:sleep(100),
    ?assertMatch(TotalUsers,
                 proplists:get_value(count, amoc_controller:users())).

user_batch_callback_receives_valid_parameters(_Config) ->
    %% GIVEN
    S = self(),
    BatchIndexWithUserCount =
        [{X, rand:uniform(10)} || X <- lists:seq(2,10)], %% {BatchIndex, UserCount}
    BatchCount = length(BatchIndexWithUserCount) + 1,
    meck:expect(test_scenario, next_user_batch,
                fun(BatchIndex, PrevUserCount) ->
                        S ! {BatchIndex, PrevUserCount},
                        {_, UserCount} =
                            lists:nth(BatchIndex,
                                      BatchIndexWithUserCount ++ [{0,0}]),
                        [{node(), UserCount, 0}]
                end),

    %% WHEN
    amoc_controller:add_batches(BatchCount, test_scenario),

    %% THEN
    [?recvAssertMatch({BatchIndex, PrevUserCount})
     || {BatchIndex, PrevUserCount} <- [{1,0}] ++ BatchIndexWithUserCount].

user_batches_respect_user_interarrival(_Config) ->
    %% GIVEN
    UserCount = 2,
    BatchCount = 1,
    Interarrival = 1000,
    BatchStrategy = [{node(), UserCount, Interarrival}],
    meck:expect(test_scenario, next_user_batch, fun(_, _) -> BatchStrategy end),

    %% WHEN
    amoc_controller:add_batches(BatchCount, test_scenario),

    %% THEN
    timer:sleep(100),
    ?assertNotMatch(UserCount, ets:info(amoc_users, size)).

user_batches_will_not_be_added_when_callback_is_not_exported(_Config) ->
    %% GIVEN
    meck:new(test_scenario_no_exports, [non_strict]),

    %% WHEN
    Res = amoc_controller:add_batches(1, test_scenario_no_exports),

    %% THEN
    ?assertMatch(Res, skip).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

remove_all_users() ->
    ets:delete_all_objects(amoc_users).

setup_amoc_controller() ->
    amoc_controller:do(test_scenario, 1, 2),
    timer:sleep(100),
    remove_all_users().

reset_amoc_controller() ->
    exit(whereis(amoc_controller), kill).
