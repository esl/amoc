-module(dummy_helper).

-include_lib("stdlib/include/assert.hrl").

-required_variable(#{name => dummy_var,
                     description => "dummy_var",
                     default_value => default_value}).

-define(comment(U), io_lib:format("Condition failed with last users distribution ~n~p", [U])).

%% amoc_dist testing function
-export([test_amoc_dist/0]).

test_amoc_dist() ->
    try
        Master = amoc_cluster:master_node(),
        Slaves = amoc_cluster:slave_nodes(),
        %% check the status of the nodes
        ?assertEqual(disabled, get_status(Master)),
        [ ?assertMatch({running, #{scenario := dummy_scenario}}, get_status(Node)) || Node <- Slaves],
        %% check user ids, users have all been started at the first two nodes
        {N1, Max1, Nodes1, Ids1, Users1} = get_users_info(Slaves),
        ?assert(N1 > 0),
        ?assertEqual(N1, Max1, ?comment(Users1)),
        ?assertEqual(Ids1, lists:seq(1, N1), ?comment(Users1)),
        [AddedNode] = Slaves -- Nodes1,
        %% add 20 users
        add_and_wait(Master, 20),
        {N2, Max2, Nodes2, Ids2, Users2} = get_users_info(Slaves),
        ?assertEqual(N2, Max2, ?comment(Users2)),
        ?assertEqual(Ids2, lists:seq(1, N2), ?comment(Users2)),
        ?assertEqual([AddedNode], Nodes2 -- Nodes1, ?comment(Users2)),
        ?assertEqual(N2, N1 + 20, ?comment(Users2)),
        %% remove 10 users
        remove_and_wait(Master, 10),
        {N3, Max3, Nodes3, _Ids3, Users3} = get_users_info(Slaves),
        ?assertEqual(N2 - 10, N3, ?comment(Users3)),
        ?assertEqual(Max2, Max3, ?comment(Users3)),
        ?assertEqual(Nodes2, Nodes3, ?comment(Users3)),
        %% try to remove N3 users
        Ret = remove_and_wait(Master, N3),
        RemovedN = lists:sum([N || {_, N} <- Ret]),
        {N4, _Max4, Nodes4, Ids4, Users4} = get_users_info(Slaves),
        ?assertEqual(N3 - RemovedN, N4, ?comment(Users4)),
        ?assertEqual(Nodes1, Nodes4, ?comment(Users4)),
        ?assert(RemovedN < N3),
        %% add 20 users
        add_and_wait(Master, 20),
        {N5, Max5, Nodes5, Ids5, Users5} = get_users_info(Slaves),
        ?assertEqual(Nodes2, Nodes5, ?comment(Users5)),
        ?assertEqual(Max5, Max2 + 20, ?comment(Users5)),
        ?assertEqual(N5, N4 + 20, ?comment(Users5)),
        ?assertEqual(Ids5 -- Ids4, lists:seq(Max2 + 1, Max5), ?comment(Users5)),
        %% terminate scenario
        stop(Master),
        [ ?assertEqual({finished, dummy_scenario}, get_status(Node)) || Node <- Slaves],
        %% return expected value
        amoc_dist_works_as_expected
    catch
        C:E:S ->
            {C, E, S}
    end.

get_users_info(SlaveNodes) ->
    Distrib = [ {Node, erpc:call(Node, amoc_users_sup, get_all_children, [])} || Node <- SlaveNodes ],
    Ids = lists:usort([Id || {_Node, Users} <- Distrib, {_, Id} <- Users]),
    Nodes = lists:usort([Node || {Node, Users} <- Distrib, [] =/= Users]),
    N = length(Ids),
    MaxId = lists:max(Ids),
    {N, MaxId, Nodes, Ids, Distrib}.

add_and_wait(Master, Num) ->
    {ok, Ret} = erpc:call(Master, amoc_dist, add, [Num]),
    timer:sleep(3000),
    Ret.

remove_and_wait(Master, Num) ->
    {ok, Ret} = erpc:call(Master, amoc_dist, remove, [Num, true]),
    timer:sleep(3000),
    Ret.

stop(Master) ->
    {ok, Ret} = erpc:call(Master, amoc_dist, stop, []),
    timer:sleep(3000),
    Ret.

get_status(Node) ->
    erpc:call(Node, amoc_controller, get_status, []).
