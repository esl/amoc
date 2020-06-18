
-module(amoc_config_attributes_SUITE).

-include_lib("eunit/include/eunit.hrl").

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
-required_variable({var6, "var6", "def6", is_list}).

%% verification functions
-export([is_atom/1, none/1]).

is_atom(_) -> true.
none(_) -> true.
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
                      {var6, "var6", "def6", is_list}],
    ?assertEqual(ExpectedResult, Result).

get_module_configuration(_) ->
    {ok, Config} = amoc_config_attributes:get_module_configuration(required_variable,
                                                                   ?MODULE, [erlang]),
    IsAtomFN = fun ?MODULE:is_atom/1,
    IsListFN = fun erlang:is_list/1,
    ?assertMatch(
        [{var0, ?MODULE, undefined, _},
         {var1, ?MODULE, def1, _},
         {var2, ?MODULE, def2, _},
         {var3, ?MODULE, def3, _},
         {var4, ?MODULE, def4, IsAtomFN},
         {var5, ?MODULE, def5, IsAtomFN},
         {var6, ?MODULE, "def6", IsListFN}],
        Config),
    [assert_fn(FN, amoc_config_attributes, 1) || {_, _, _, FN, _} <- Config,
                                                 FN =/= IsAtomFN, FN =/= IsListFN].

assert_fn(FN, Module, Arity) ->
    ?assertEqual({arity, Arity}, erlang:fun_info(FN, arity)),
    ?assertEqual({module, Module}, erlang:fun_info(FN, module)).

errors_reporting(_) ->
    Attributes = [
        {"invalid_var0", "var0"},
        {invalid_var1, <<"var1">>, def1},
        {invalid_var2, "var2", def2, <<"invalid_verification_method">>},
        {valid_var3, "var3", def3, [def3, another_atom]},
        {invalid_var4, "var4", def4, not_exported_function}],
    {error, invalid_attribute_format, Reason} =
        amoc_config_attributes:process_module_attributes([?MODULE], ?MODULE, Attributes),
    ?assertEqual(
        [{invalid_attribute, {"invalid_var0", "var0"}},
         {invalid_attribute, {invalid_var1, <<"var1">>, def1}},
         {invalid_verification_method,
            {invalid_var2, ?MODULE, def2, <<"invalid_verification_method">>}},
         {verification_method_not_exported,
            {invalid_var4, ?MODULE, def4, not_exported_function},
            [?MODULE]}],
        Reason).