
-module(amoc_config_attributes_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("../src/amoc_config/amoc_config.hrl").

-export([all/0]).
-export([get_module_attributes/1,
         get_module_configuration/1,
         errors_reporting/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% these attributes and functions are required for the testing purposes %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-required_variable({var0, "var0"}).
-required_variable({var1, "var1", def1}).
-required_variable([
    {var2, "var2", def2, none},
    {var3, "var3", def3, [def3, another_atom]},
    {var4, "var4", def4, is_atom},
    {var5, "var5", def5, fun ?MODULE:is_atom/1}
]).
-required_variable({var6, "var6", "def6", is_list, read_only}).
-required_variable({var7, "var7", def7, none, none}).
-required_variable([
    {var8, "var8", def8, none, update_value},
    {var9, "var9", def9, none, fun ?MODULE:update_value/2}
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
     errors_reporting].

get_module_attributes(_) ->
    Result = amoc_config_attributes:get_module_attributes(required_variable, ?MODULE),
    ExpectedResult = [{var0, "var0"},
                      {var1, "var1", def1},
                      {var2, "var2", def2, none},
                      {var3, "var3", def3, [def3, another_atom]},
                      {var4, "var4", def4, is_atom},
                      {var5, "var5", def5, fun ?MODULE:is_atom/1},
                      {var6, "var6", "def6", is_list, read_only},
                      {var7, "var7", def7, none, none},
                      {var8, "var8", def8, none, update_value},
                      {var9, "var9", def9, none, fun ?MODULE:update_value/2}],
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
    Attributes = [
        {"invalid_var0", "var0"},
        {invalid_var1, <<"var1">>, def1},
        {invalid_var2, "var2", def2, <<"invalid_verification_method">>},
        {valid_var3, "var3", def3, [def3, another_atom]},
        {invalid_var4, "var4", def4, not_exported_function},
        {invalid_var5, "var5", def5, is_atom, {invalid_update_method}},
        {invalid_var6, "var6", def6, is_atom, not_exported_function}],
    {error, invalid_attribute_format, Reason} =
        amoc_config_attributes:process_module_attributes([?MODULE], ?MODULE, Attributes),
    ?assertEqual(
        [{invalid_attribute, {"invalid_var0", "var0"}},
         {invalid_attribute, {invalid_var1, <<"var1">>, def1}},
         {invalid_verification_method,
          {invalid_var2, "var2", def2, <<"invalid_verification_method">>}},
         {verification_method_not_exported,
          {invalid_var4, "var4", def4, not_exported_function},
          [?MODULE]},
         {invalid_update_method,
          {invalid_var5, "var5", def5, is_atom, {invalid_update_method}}},
         {update_method_not_exported,
          {invalid_var6, "var6", def6, is_atom, not_exported_function},
          [?MODULE]}],
        Reason).