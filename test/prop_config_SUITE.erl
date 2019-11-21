-module(prop_config_SUITE).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [%test_any_type,
     config_prop_test].

test_any_type(_Config) ->
    ProperTest = ?FORALL(Value, any(), not is_map(Value)),
    ?assertEqual(true, proper:quickcheck(ProperTest, [long_result, 10000, quiet])).

config_prop_test(_Config) ->
     ?assertEqual(true, proper:quickcheck(config_prop(), [long_result, 1000, quiet])).

config_prop() ->
    ?FORALL(Cmds, proper_statem:commands(?MODULE),
            begin
                {_History, State, Result} = proper_statem:run_commands(?MODULE, Cmds),
                unset_all(State),
                Result =:= ok
            end).

key() ->
    oneof([a, b, c, d, e, f, g]).

real_any() ->
    %% ensure maps are tested!!!
    weighted_union([{1, map(any(), any())},
                    {10, any()}]).

%% Abstract state machine
initial_state() ->
    #{vars => [], envs => []}.

command(_S) ->
    oneof([
              {call, ?MODULE, set_app_env_variable, [key(), real_any()]},
              {call, amoc_config_env, get, [key()]},
              {call, amoc_config_env, get, [key(), real_any()]},
              {call, ?MODULE, set_env_variable, [key(), real_any()]}
          ]).

%%
precondition(_S, _) ->
    true.

%%
next_state(S=#{vars := Vars}, _Res, {call, ?MODULE, set_app_env_variable, 
                                        [Key, Value]}) ->
    ConvertedKey = lists:flatten(io_lib:format("~p", [Key])),
    case os:getenv(env_name(ConvertedKey)) of
        false ->
            S#{vars := lists:keystore(Key, 1, Vars, {Key, Value})};
        _ ->
            S
    end;

next_state(S=#{vars := Vars, envs := Envs}, _Res,
           {call, ?MODULE, set_env_variable, [Key, Value]}) ->
    S#{vars := lists:keystore(Key, 1, Vars, {Key, Value}),
       envs := [Key | Envs]};

next_state(S, _Res, {call, _, _, _}) ->
    S.

%%
postcondition(#{vars := Vars}, {call, amoc_config_env, get, [Key]}, Res) ->
    proplists:get_value(Key, Vars) =:= Res;

postcondition(#{vars := Vars}, {call, amoc_config_env, get, [Key, Default]}, Res) ->
    proplists:get_value(Key, Vars, Default) =:= Res;

postcondition(_S, {call, ?MODULE, _, _}, _Res) ->
    true.

%% statem helpers
set_env_variable(Name, Value) ->
    EnvName = env_name(Name),
    StringValue=lists:flatten(io_lib:format("~p", [Value])),
    %% io:format("~s",[StringValue]),
    true = os:putenv(EnvName, StringValue),
    ok.

set_app_env_variable(Name, Value) ->
    ok = application:set_env(amoc, Name, Value).


unset_all(#{envs := Envs}) ->
    [ true = os:unsetenv(env_name(Env)) || Env <- Envs ],
    [ ok = application:unset_env(amoc, Name)
      || {Name, _} <- application:get_all_env(amoc) ].


env_name(Atom) when is_atom(Atom) ->
    "AMOC_" ++ string:uppercase(atom_to_list(Atom));
env_name(String) when is_list(String) ->
    "AMOC_" ++ string:uppercase(String).
