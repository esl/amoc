-module(config_SUITE).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() -> [].

groups() -> [].

all() ->
    [config_osenv_test,
     config_prop_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_group, Config) ->
    Config.

end_per_group(_group, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

config_osenv_test(_Config) ->
    given_osenv_set([{"AMOC_interarrival", "100"},
                     {"AMOC_something", "[{a,b,c}, 9, 8, {7}]"}]),
    given_amoc_started(),
    ?assertEqual(100, amoc_config:get(interarrival)),
    ?assertEqual(60000, amoc_config:get(repeat_interval)),
    ?assertEqual(something, amoc_config:get(anything, something)),
    ?assertEqual([{a,b,c},9,8,{7}], amoc_config:get(something)).

given_amoc_started() ->
    {ok, _} = application:ensure_all_started(amoc).

given_osenv_set(Envs) ->
    [ true = os:putenv(Name, Value) || {Name, Value} <- Envs ].

config_prop_test(_Config) ->
     ?assertEqual(true, proper:quickcheck(config_prop(), [long_result, 100])).

config_prop() ->
    ?FORALL(Cmds, proper_statem:commands(?MODULE),
            begin
                {_History, State, Result} = proper_statem:run_commands(?MODULE, Cmds),
                unset_all(State),
                Result =:= ok
            end).

key() ->
    oneof([a, b, c, d, e, f, g]).

%% Abstract state machine
initial_state() ->
    #{vars => [], envs => []}.

command(_S) ->
    oneof([
           {call, amoc_config, set, [key(), any()]},
           {call, amoc_config, get, [key()]},
           {call, amoc_config, get, [key(), any()]},
           {call, ?MODULE, set_env_variable, [key(), any()]},
           {call, ?MODULE, safe_fetch, [key()]}
           ]).

%%
precondition(_S, _) ->
    true.

%%
next_state(S=#{vars := Vars}, _Res, {call, amoc_config, set, [Key, Value]}) ->
    S#{vars := lists:keystore(Key, 1, Vars, {Key, Value})};

next_state(S=#{vars := Vars, envs := Envs}, _Res,
           {call, ?MODULE, set_env_variable, [Key, Value]}) ->
    S#{vars := lists:keystore(Key, 1, Vars, {Key, Value}),
       envs := [Key | Envs]};

next_state(S, _Res, {call, _, _, _}) ->
    S.

%%
postcondition(#{vars := Vars}, {call, amoc_config, get, [Key]}, Res) ->
    proplists:get_value(Key, Vars) =:= Res;

postcondition(#{vars := Vars}, {call, amoc_config, get, [Key, Default]}, Res) ->
    proplists:get_value(Key, Vars, Default) =:= Res;

postcondition(#{vars := Vars}, {call, amoc_config, safe_fetch, [Key]},
              {key_not_found, Key}) ->
    false =:= lists:keyfind(Key, 1, Vars);

postcondition(#{vars := Vars}, {call, amoc_config, safe_fetch, [Key]}, Res) ->
    {Key, Value} = lists:keyfind(Key, 1, Vars),
    Value =:= Res;

postcondition(_S, {call, _, _, _}, _Res) ->
    true.

%% statem helpers
set_env_variable(Name, Value) ->
    EnvName = "AMOC_" ++ atom_to_list(Name),
    true = os:putenv(EnvName, lists:flatten(io_lib:format("~p", [Value]))),
    %% Reset only this env variable in the application
    ok = amoc_config:set_env_variables(os:getenv(), fun(N) -> N =:= Name end).

safe_fetch(Name) ->
    catch amoc_config:fetch(Name).

unset_all(#{envs := Envs}) ->
    [ true = os:unsetenv("AMOC_" ++ atom_to_list(Env)) || Env <- Envs ],
    [ ok = application:unset_env(amoc, Name)
      || {Name, _} <- application:get_all_env(amoc) ].
