
-module(amoc_config_attributes_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("../src/amoc_config/amoc_config.hrl").

-export([all/0]).
-export([get_module_attributes/1,
         get_module_configuration/1,
         errors_reporting/1,
         multiple_exports_error/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes and functions are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable(#{name => var0, description => "var0"}).
-required_variable(#{name => var1, description => "var1", value => def1}).
-required_variable([
    #{name => var2, description => "var2", value => def2, verification => none},
    #{name => var3, description => "var3", value => def3,
      verification => [def3, another_atom]},
    #{name => var4, description => "var4", value => def4, verification => is_atom},
    #{name => var5, description => "var5", value => def5,
      verification => fun ?MODULE:is_atom/1}
]).
-required_variable(#{name => var6, description => "var6", value => "def6",
                     verification => is_list, update => read_only}).
-required_variable(#{name => var7, description => "var7", value => def7,
                     verification => none, update => none}).
-required_variable([
    #{name => var8, description => "var8", value => def8,
      verification => none, update => update_value},
    #{name => var9, description => "var9", value => def9,
      verification => none, update => fun ?MODULE:update_value/2}
]).

%% verification functions
-export([is_atom/1, none/1]).

%% update functions
-export([update_value/2, none/2]).

is_atom(_) -> true.
none(_) -> true.

update_value(_, _) -> ok.
none(_, _) -> ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [get_module_attributes,
     get_module_configuration,
     errors_reporting,
     multiple_exports_error].

get_module_attributes(_) ->
    Result = amoc_config_attributes:get_module_attributes(required_variable, ?MODULE),
    ExpectedResult = [
        #{name => var0, description => "var0"},
        #{name => var1, description => "var1", value => def1},
        #{name => var2, description => "var2", value => def2, verification => none},
        #{name => var3, description => "var3", value => def3,
          verification => [def3, another_atom]},
        #{name => var4, description => "var4", value => def4, verification => is_atom},
        #{name => var5, description => "var5", value => def5,
          verification => fun ?MODULE:is_atom/1},
        #{name => var6, description => "var6", value => "def6",
          verification => is_list, update => read_only},
        #{name => var7, description => "var7", value => def7,
          verification => none, update => none},
        #{name => var8, description => "var8", value => def8,
          verification => none, update => update_value},
        #{name => var9, description => "var9", value => def9,
          verification => none, update => fun ?MODULE:update_value/2}],
    ?assertEqual(ExpectedResult, Result).

get_module_configuration(_) ->
    {ok, Config} = amoc_config_attributes:get_module_configuration(required_variable,
                                                                   ?MODULE, [erlang]),
    IsAtomFN = fun ?MODULE:is_atom/1,
    IsListFN = fun erlang:is_list/1,
    UpdateValueFN = fun ?MODULE:update_value/2,
    ?assertMatch(
        [#module_parameter{name = var0, mod = ?MODULE, value = undefined,
                           update_fn = read_only},
         #module_parameter{name = var1, mod = ?MODULE, value = def1,
                           update_fn = read_only},
         #module_parameter{name = var2, mod = ?MODULE, value = def2,
                           update_fn = read_only},
         #module_parameter{name = var3, mod = ?MODULE, value = def3,
                           update_fn = read_only},
         #module_parameter{name = var4, mod = ?MODULE, value = def4,
                           verification_fn = IsAtomFN, update_fn = read_only},
         #module_parameter{name = var5, mod = ?MODULE, value = def5,
                           verification_fn = IsAtomFN, update_fn = read_only},
         #module_parameter{name = var6, mod = ?MODULE, value = "def6",
                           verification_fn = IsListFN, update_fn = read_only},
         #module_parameter{name = var7, mod = ?MODULE, value = def7},
         #module_parameter{name = var8, mod = ?MODULE, value = def8,
                           update_fn = UpdateValueFN},
         #module_parameter{name = var9, mod = ?MODULE, value = def9,
                           update_fn = UpdateValueFN}],
        Config),
    [assert_fn(FN, amoc_config_attributes, 1)
     || #module_parameter{verification_fn = FN} <- Config,
        FN =/= IsAtomFN, FN =/= IsListFN],
    [assert_fn(FN, amoc_config_attributes, 2)
     || #module_parameter{update_fn = FN} <- Config,
        FN =/= UpdateValueFN, FN =/= read_only].

assert_fn(FN, Module, Arity) ->
    ?assertEqual({arity, Arity}, erlang:fun_info(FN, arity)),
    ?assertEqual({module, Module}, erlang:fun_info(FN, module)).

errors_reporting(_) ->
    InvalidParam0 = #{name => "invalid_var0", description => "var0"},
    InvalidParam1 = #{name => invalid_var1, description => [$a, <<"b">>]},
    InvalidParam2 = #{name => invalid_var2, description => "var2",
                      verification => <<"invalid_verification_method">>},
    ValidParam3 = #{name => valid_var3, description => "var3", value => def3,
                    verification => [def3, another_atom]},
    InvalidParam4 = #{name => invalid_var4, description => "var4",
                      verification => not_exported_function},
    InvalidParam5 = #{name => invalid_var5, description => "var5",
                      update => {invalid_update_method}},
    InvalidParam6 = #{name => invalid_var6, description => "var6",
                      update => not_exported_function},
    Attributes = [InvalidParam0, InvalidParam1, InvalidParam2,ValidParam3,
                  InvalidParam4, InvalidParam5,InvalidParam6],
    {error, invalid_attribute_format, Reason} =
        amoc_config_attributes:process_module_attributes([mod1, mod2], ?MODULE, Attributes),
    ?assertEqual(
        [{invalid_attribute, InvalidParam0},
         {invalid_attribute, InvalidParam1},
         {invalid_verification_method, InvalidParam2},
         {verification_method_not_exported, InvalidParam4, [?MODULE, mod1, mod2]},
         {invalid_update_method, InvalidParam5},
         {update_method_not_exported, InvalidParam6, [?MODULE, mod1, mod2]}],
        Reason).

multiple_exports_error(_)->
    Module = ct, %% use some dummy module that belongs to some application
    {ok, App} = application:get_application(Module),
    Param = #{name => var0, description => "var0", verification=>add},
    AllVerificationModules = [amoc, amoc_dist],
    [code:ensure_loaded(Module) || Module <- AllVerificationModules],
    {error, invalid_attribute_format, Reason} =
        amoc_config_attributes:process_module_attributes(
            AllVerificationModules, Module, [Param]),
    ?assertEqual(
        [{multiple_functions_found,
          [fun amoc_dist:add/1, fun amoc:add/1],
          #{description => "var0", name => var0, verification => add}}],
        Reason),
    %% now try to set 'config_verification_modules' env variable for App
    amoc_config_helper:set_app_env(App, config_verification_modules, [amoc, amoc]),
    {ok, Result} = amoc_config_attributes:process_module_attributes(
                       AllVerificationModules, Module, [Param]),
    ?assertEqual(
        [{module_parameter, var0, ct, undefined, fun amoc:add/1, read_only}],
        Result),
    amoc_config_helper:unset_app_env(App, config_verification_modules).
