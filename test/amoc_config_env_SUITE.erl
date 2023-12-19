-module(amoc_config_env_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(amoc_config_helper, [format_value/1,
                             get_env/1,
                             get_env/2,
                             set_os_env/2,
                             unset_os_env/1,
                             set_empty_os_env/1]).

-compile(export_all).

-define(MOCK_MOD, custom_parser).

all() ->
    [parse_value_prop_test,
     default_parser_test,
     valid_custom_parser_test,
     invalid_custom_parser_test].

init_per_suite(Config) ->
    %% set some predefined variables
    set_os_env(set_var, some_value),
    unset_os_env(unset_var),
    set_empty_os_env(empty_var),
    ct:pal("OS env:~n   ~p", [os:getenv()]),
    Config.

end_per_suite(_) ->
    %% unset predefined variables
    unset_os_env(set_var),
    unset_os_env(empty_var).

init_per_testcase(valid_custom_parser_test, Config) ->
    meck:new(?MOCK_MOD, [non_strict, no_link]),
    set_config_parser_module(?MOCK_MOD),
    ct:pal("AMOC_* OS env. variables:~n   ~p",
           [[AmocEnv || "AMOC_" ++ _ = AmocEnv <- os:getenv()]]),
    Config;
init_per_testcase(invalid_custom_parser_test, Config) ->
    %% setting non-existing config parser module
    set_config_parser_module(invalid_parser_module),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(valid_custom_parser_test, _Config) ->
    unset_config_parser_module(),
    meck:unload();
end_per_testcase(invalid_custom_parser_test, _Config) ->
    unset_config_parser_module();
end_per_testcase(_, _Config) ->
    ok.
%%-----------------------------------------------------------------------------------
%% test cases
%%-----------------------------------------------------------------------------------

parse_value_prop_test(_) ->
    RealAnyType = weighted_union([{1, map(any(), any())},
                                  {10, any()}]),
    ProperTest = ?FORALL(Value, RealAnyType,
                         amoc_config_parser:parse_value(format_value(Value)) =:= {ok, Value}),
    ?assertEqual(true, proper:quickcheck(ProperTest, [quiet])).

default_parser_test(_) ->
    %% testing default config parser module (amoc_config_parser)
    ?assertEqual(some_value, get_env(set_var)),
    ?assertEqual(some_value, get_env(set_var, default_value)),
    ?assertEqual(undefined, get_env(unset_var)),
    ?assertEqual(default_value, get_env(unset_var, default_value)),
    ?assertEqual(undefined, get_env(empty_var)),
    ?assertEqual(default_value, get_env(empty_var, default_value)).

valid_custom_parser_test(_) ->
    %% testing invalid custom config parser module
    Self = self(),

    %% successful parsing
    meck:expect(?MOCK_MOD, parse_value, ['_'], {ok, another_value}),
    ?assertEqual(another_value, get_env(set_var)),
    ?assertEqual(another_value, get_env(set_var, default_value)),
    OkCall = {Self, {?MOCK_MOD, parse_value, ["some_value"]}, {ok, another_value}},
    ?assertEqual([OkCall, OkCall], meck:history(?MOCK_MOD)),
    meck:reset(?MOCK_MOD),

    %% gracefully failing parsing
    meck:expect(?MOCK_MOD, parse_value, ['_'], {error, some_error}),
    ?assertEqual(undefined, get_env(set_var)),
    ?assertEqual(default_value, get_env(set_var, default_value)),
    ErrCall = {Self, {?MOCK_MOD, parse_value, ["some_value"]}, {error, some_error}},
    ?assertEqual([ErrCall, ErrCall], meck:history(?MOCK_MOD)),
    meck:reset(?MOCK_MOD),

    %% invalid parsing return value
    meck:expect(?MOCK_MOD, parse_value, ['_'], invalid_ret_value),
    ?assertEqual(undefined, get_env(set_var)),
    ?assertEqual(default_value, get_env(set_var, default_value)),
    InvRetValCall = {Self, {?MOCK_MOD, parse_value, ["some_value"]}, invalid_ret_value},
    ?assertEqual([InvRetValCall, InvRetValCall], meck:history(?MOCK_MOD)),
    meck:reset(?MOCK_MOD),

    %% crash during parsing
    meck:expect(?MOCK_MOD, parse_value, fun(_) -> error(bang) end),
    ?assertEqual(undefined, get_env(set_var)),
    ?assertEqual(default_value, get_env(set_var, default_value)),
    ?assertMatch([{Self, {?MOCK_MOD, parse_value, ["some_value"]}, error, bang, _},
                  {Self, {?MOCK_MOD, parse_value, ["some_value"]}, error, bang, _}],
                 meck:history(?MOCK_MOD)),
    meck:reset(?MOCK_MOD),

    %% unset and empty env variables (custom parsing module is not triggered)
    ?assertEqual(undefined, get_env(unset_var)),
    ?assertEqual(default_value, get_env(unset_var, default_value)),
    ?assertEqual(undefined, get_env(empty_var)),
    ?assertEqual(default_value, get_env(empty_var, default_value)),
    ?assertEqual([], meck:history(?MOCK_MOD)).

invalid_custom_parser_test(_) ->
    %% testing invalid custom config parser module
    ?assertEqual(undefined, get_env(set_var)),
    ?assertEqual(default_value, get_env(set_var, default_value)),
    ?assertEqual(undefined, get_env(unset_var)),
    ?assertEqual(default_value, get_env(unset_var, default_value)),
    ?assertEqual(undefined, get_env(empty_var)),
    ?assertEqual(default_value, get_env(empty_var, default_value)).

%%-----------------------------------------------------------------------------------
%% helper functions
%%-----------------------------------------------------------------------------------

set_config_parser_module(Mod) ->
    application:set_env(amoc, config_parser_mod, Mod).

unset_config_parser_module() ->
    application:unset_env(amoc, config_parser_mod).
