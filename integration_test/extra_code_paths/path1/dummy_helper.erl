-module(dummy_helper).

-required_variable(#{name=>dummy_var, description=>"dummy_var",
                     default_value=>default_value}).

%% amoc_dist testing function
-export([test_amoc_dist/0]).

test_amoc_dist() ->
    try
        Master = amoc_cluster:master_node(),
        Slaves = amoc_cluster:slave_nodes(),
        %% check the status of the nodes
        disabled = rpc:call(Master, amoc_controller, get_status, []),
        [{running, dummy_scenario, _, _} = rpc:call(Node, amoc_controller, get_status, [])
         || Node <- Slaves],
        %% check user ids
        {N1, Nodes1, Ids1, Max1} = get_users_info(Slaves),
        true = N1 > 0,
        N1 = Max1,
        Ids1 = lists:seq(1, N1),
        [AddedNode] = Slaves -- Nodes1,
        %% add 20 users
        {ok, _} = rpc:call(Master, amoc_dist, add, [20]),
        timer:sleep(3000),
        {N2, Nodes2, Ids2, Max2} = get_users_info(Slaves),
        N2 = Max2,
        Ids2 = lists:seq(1, N2),
        [AddedNode] = Nodes2 -- Nodes1,
        N2 = N1 + 20,
        %% remove 10 users
        {ok, _} = rpc:call(Master, amoc_dist, remove, [10, true]),
        timer:sleep(3000),
        {N3, Nodes3, _Ids3, Max3} = get_users_info(Slaves),
        Nodes2 = Nodes3,
        Max3 = Max2,
        N2 = N3 + 10,
        %% try to remove N3 users
        {ok, Ret} = rpc:call(Master, amoc_dist, remove, [N3, true]),
        RemovedN = lists:sum([N || {_, N} <- Ret]),
        timer:sleep(3000),
        {N4, Nodes4, Ids4, _Max4} = get_users_info(Slaves),
        Nodes1 = Nodes4,
        N3 = N4 + RemovedN,
        true = RemovedN < N3,
        %% add 20 users
        {ok, _} = rpc:call(Master, amoc_dist, add, [20]),
        timer:sleep(3000),
        {N5, Nodes5, Ids5, Max5} = get_users_info(Slaves),
        Nodes2 = Nodes5,
        Max5 = Max2 + 20,
        N5 = N4 + 20,
        true = Ids5 -- Ids4 =:= lists:seq(Max2 + 1, Max5),
        %% terminate scenario
        {ok,_} = rpc:call(Master, amoc_dist, stop, []),
        timer:sleep(3000),
        [{finished, dummy_scenario} = rpc:call(Node, amoc_controller, get_status, [])
         || Node <- Slaves],
        %% return expected value
        amoc_dist_works_as_expected
    catch
        C:E:S ->
            {error, {C, E, S}}
    end.

get_users_info(SlaveNodes) ->
    Users = [{Node, Id} ||
        Node <- SlaveNodes,
        {Id, _Pid} <- rpc:call(Node, ets, tab2list, [amoc_users])],
    Ids = lists:usort([Id || {_, Id} <- Users]),
    Nodes = lists:usort([Node || {Node, _} <- Users]),
    N = length(Ids),
    N = length(Users),
    MaxId = lists:max(Ids),
    {N, Nodes, Ids, MaxId}.
