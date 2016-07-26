-module(prop_config).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [config_prop_test].

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
           {call, ?MODULE, set_app_env_variable, [key(), any()]},
           {call, amoc_config, get, [key()]},
           {call, amoc_config, get, [key(), any()]},
           {call, ?MODULE, set_env_variable, [key(), any()]},
           {call, ?MODULE, safe_fetch, [key()]}
           ]).

%%
precondition(_S, _) ->
    true.

%%
next_state(S=#{vars := Vars}, _Res, {call, ?MODULE, set_app_env_variable, 
                                        [Key, Value]}) ->
    ConvertedKey = lists:flatten(io_lib:format("~p", [Key])),
    case os:getenv("AMOC_" ++ ConvertedKey) of
        false ->
            S#{vars := lists:keystore(Key, 1, Vars, {Key, Value})};
        ValueOther ->
            S
    end;

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
    ok.

set_app_env_variable(Name, Value) ->
    ok = application:set_env(amoc, Name, Value).

safe_fetch(Name) ->
    catch amoc_config:fetch(Name).

unset_all(#{envs := Envs}) ->
    [ true = os:unsetenv("AMOC_" ++ atom_to_list(Env)) || Env <- Envs ],
    [ ok = application:unset_env(amoc, Name)
      || {Name, _} <- application:get_all_env(amoc) ].
