-module(dummy_scenario).
-behaviour(amoc_scenario).

%% var1 must be equal to one of the values in the verification list
%% and the default value is def1
-required_variable(#{name => var1, description => "description1",
                     default_value => def1,
                     verification => [def1, another_value]}).

-required_variable([
    %% var2 must be positively verified by the test_verification_function/1 function.
    %% this function must be exported from the scenario module, or from any of
    %% the modules specified in the config_verification_modules erlang application
    %% configuration variable.
    #{name => var2, description => "description2", default_value => def2,
      verification => test_verification_function},
    %% alternatively the verification function can be supplied as a function pointer
    %% in a 'fun module:function/arity' format. Note that it must be an exported function.
    %% usage of 'fun function/arity' format in the module attribute simply will
    %% not pass compilation.
    #{name => var3, description => "description3", default_value => def3,
      verification => fun ?MODULE:test_verification_function/1}]).

%% 'none' is a predefined verification function which accepts all the values
-required_variable(#{name => var4, description => "description4",
                     default_value => def4,
                     verification => none}).

-required_variable([
    %% when verification method is not set, it defaults to `none`
    #{name => var5, description => "description5", default_value => def5},
    %% when value is not set, it defaults to `undefined`
    #{name => var6, description => "description6"},
    #{name => nodes, description => "this variable is set for docker "
                                    "container via AMOC_NODES env"},
    #{name => test, description => "this one to be set via REST API"}]).

%% parameter verification method
-export([test_verification_function/1]).

%% amoc_scenario behaviour
-export([init/0, start/1]).

%% amoc_dist testing function
-export([test_amoc_dist/0]).

test_verification_function(def2) -> true;
test_verification_function(_)    -> {true, new_value}.

-spec init() -> ok.
init() ->
    %% amoc follows a couple of rules during the scenario initialisation:
    %%  - if any parameter verification fails, amoc will not start
    %%    the scenario and the init/0 function is not triggered.
    %%  - if the init/0 function fails, amoc will not start any users (by
    %%    calling a start/1 or start2 function)
    %% if the REST API reports that scenario is executed, than all the
    %% initialisation steps described above have passed successfully
    def1 = amoc_config:get(var1, def_value),
    def2 = amoc_config:get(var2, def_value),
    new_value = amoc_config:get(var3, def_value),
    def4 = amoc_config:get(var4, def_value),
    def5 = amoc_config:get(var5, def_value),
    def_value = amoc_config:get(var6, def_value),
    undefined = amoc_config:get(var6),
    [_ | _] = amoc_config:get(nodes),
    %% it doesn't matter if an undeclared_variable is passed through the
    %% os or erlang app environment variable. if it's not declared using
    %% the -required_variable(...) attribute, then any attempt to get it
    %% results in exception.
    {invalid_setting, undeclared_variable} =
        (catch amoc_config:get(undeclared_variable)),
    %% this variable is set via REST API
    <<"test_value">> = amoc_config:get(test),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(_Id) ->
    %%sleep 15 minutes
    timer:sleep(1000 * 60 * 15),
    amoc_user:stop().


test_amoc_dist() ->
    try
        Master = amoc_cluster:master_node(),
        Slaves = amoc_cluster:slave_nodes(),
        %% check the status of the nodes
        disabled = rpc:call(Master, amoc_controller, get_status, []),
        [{running, ?MODULE, _, _} = rpc:call(Node, amoc_controller, get_status, []) ||
            Node <- Slaves],
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
        [{finished, ?MODULE} = rpc:call(Node, amoc_controller, get_status, []) ||
            Node <- Slaves],
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
