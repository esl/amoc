%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% this module is responsible for parsing module attributes
%%==============================================================================
-module(amoc_config_attributes).

-include("amoc_config.hrl").

%% API
-export([get_module_configuration/3]).
-export([none/1, none/2]).
-ifdef(TEST).
-export([%% exported for testing only
         get_module_attributes/2,
         process_module_attributes/3]).
-endif.

-type maybe_module_attribute() :: module_attribute() | term().
-type maybe_verification_method() :: verification_method() | term().
-type maybe_update_method() :: update_method() | term().

-type attribute_name() :: required_variable | override_variable.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get_module_configuration(attribute_name(), module(), [module()]) ->
    {ok, module_configuration()} | amoc_config_utils:error().
get_module_configuration(AttrName, Module, VerificationModules) ->
    try
        ScenarioAttributes = get_module_attributes(AttrName, Module),
        process_module_attributes(VerificationModules, Module, ScenarioAttributes)
    catch
        _:_ -> {error, invalid_module, Module}
    end.

-spec none(any()) -> true.
none(_) -> true.

-spec none(any(), any()) -> ok.
none(_, _) -> ok.
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_module_attributes(attribute_name(), module()) -> [maybe_module_attribute()].
get_module_attributes(AttrName, Module) ->
    ModuleAttributes = apply(Module, module_info, [attributes]),
    RequiredVariables = proplists:get_all_values(AttrName, ModuleAttributes),
    lists:append(RequiredVariables).

-spec process_module_attributes([module()], module(), [maybe_module_attribute()]) ->
    {ok, module_configuration()} | amoc_config_utils:error().
process_module_attributes(VerificationModules, Module, ScenarioAttributes) ->
    Config = [process_var_attr(VerificationModules, Module, Attr)
              || Attr <- ScenarioAttributes],
    amoc_config_utils:maybe_error(invalid_attribute_format, Config).

-spec process_var_attr([module()], module(), maybe_module_attribute()) ->
    {ok, module_parameter()} | {error, reason()}.
process_var_attr(VerificationModules, Module, Attr) ->
    PipelineActions = [
        {fun check_mandatory_fields/1, []},
        {fun check_default_value/1, []},
        {fun check_verification_method/4, [VerificationModules, Module, Attr]},
        {fun check_update_method/4, [VerificationModules, Module, Attr]},
        {fun make_module_parameter/2, [Module]}],
    amoc_config_utils:pipeline(PipelineActions, {ok, Attr}).

-spec check_mandatory_fields(maybe_module_attribute()) ->
    {ok, maybe_module_attribute()} | {error, reason()}.
check_mandatory_fields(#{description := List, name := Atom} = Attr) when is_atom(Atom),
                                                                         is_list(List) ->
    case io_lib:char_list(List) of
        true -> {ok, Attr};
        false -> {error, {invalid_attribute, Attr}}
    end;
check_mandatory_fields(Attr) ->
    {error, {invalid_attribute, Attr}}.

-spec check_default_value(maybe_module_attribute()) -> {ok, maybe_module_attribute()}.
check_default_value(Attr) ->
    DefaultValue = maps:get(value, Attr, undefined),
    {ok, Attr#{value => DefaultValue}}.

-spec check_verification_method(maybe_module_attribute(), [module()],
                                module(), maybe_module_attribute()) ->
    {ok, maybe_module_attribute()} | {error, reason()}.
check_verification_method(Attr, VerificationModules, Module, OriginalAttr) ->
    VerificationMethod = maps:get(verification, Attr, none),
    case verification_fn(VerificationModules, Module, VerificationMethod) of
        not_exported ->
            {error, {verification_method_not_exported, OriginalAttr,
                     [Module | VerificationModules]}};
        invalid_method ->
            {error, {invalid_verification_method, OriginalAttr}};
        {multiple_functions_found, Functions} ->
            {error, {multiple_functions_found, Functions, OriginalAttr}};
        VerificationFn ->
            {ok, Attr#{verification => VerificationFn}}
    end.

-spec check_update_method(maybe_module_attribute(), [module()],
                          module(), maybe_module_attribute()) ->
    {ok, maybe_module_attribute()} | {error, reason()}.
check_update_method(Attr, VerificationModules, Module, OriginalAttr) ->
    UpdateMethod = maps:get(update, Attr, read_only),
    case update_fn(VerificationModules, Module, UpdateMethod) of
        not_exported ->
            {error, {update_method_not_exported, OriginalAttr,
                     [Module | VerificationModules]}};
        invalid_method ->
            {error, {invalid_update_method, OriginalAttr}};
        {multiple_functions_found, Functions} ->
            {error, {multiple_functions_found, Functions, OriginalAttr}};
        UpdateFn ->
            {ok, Attr#{update => UpdateFn}}
    end.

-spec make_module_parameter(maybe_module_attribute(), module()) ->
    {ok, module_parameter()}.
make_module_parameter(#{name := Name, value := Value, update := UpdateFn,
                        verification := VerificationFn}, Module) ->
    {ok, #module_parameter{name = Name, mod = Module, value = Value,
                           verification_fn = VerificationFn, update_fn = UpdateFn}}.

-spec verification_fn([module()], module(), maybe_verification_method()) ->
    maybe_verification_fun() | not_exported | invalid_method
    | {multiple_functions_found, [maybe_verification_fun()]}.
verification_fn(_, _, none) ->
    fun ?MODULE:none/1;
verification_fn(_, _, [_ | _] = OneOF) ->
    one_of_fun(OneOF);
verification_fn(_, _, Fun) when is_function(Fun, 1) ->
    Fun;
verification_fn(VerificationModules, Module, Atom) when is_atom(Atom) ->
    select_function(VerificationModules, Module, Atom, 1);
verification_fn(_, _, _) ->
    invalid_method.

-spec update_fn([module()], module(), maybe_update_method()) ->
    maybe_update_fun() | not_exported | invalid_method
    | read_only | {multiple_functions_found, [maybe_update_fun()]}.
update_fn(_, _, read_only) ->
    read_only;
update_fn(_, _, none) ->
    fun ?MODULE:none/2;
update_fn(_, _, Fun) when is_function(Fun, 2) ->
    Fun;
update_fn(VerificationModules, Module, Atom) when is_atom(Atom) ->
    select_function(VerificationModules, Module, Atom, 2);
update_fn(_, _, _) ->
    invalid_method.

-spec select_function([module()], module(), atom(), non_neg_integer()) ->
    function() | not_exported | {multiple_functions_found, [function()]}.
select_function(VerificationModules, Module, FunctionName, Arity) ->
    %% first try to find exported function in the current module, then
    %% check config_verification_modules defined for the application of
    %% the current module, then try config_verification_modules defined
    %% in all other applications.
    AppVerificationModules = get_app_verification_modules(Module),
    OtherVerificationModules = VerificationModules -- AppVerificationModules,
    ModuleSets = [[Module], AppVerificationModules, OtherVerificationModules],
    select_function(ModuleSets, FunctionName, Arity).

-spec select_function([[module()]], atom(), non_neg_integer()) ->
    function() | not_exported | {multiple_functions_found, [function()]}.
select_function([], _FunctionName, _Arity) ->
    not_exported;
select_function([Modules | T], FunctionName, Arity) ->
    case is_exported(Modules, [], FunctionName, Arity) of
        not_exported -> select_function(T, FunctionName, Arity);
        ReturnValue -> ReturnValue
    end.

-spec get_app_verification_modules(module()) -> [module()].
get_app_verification_modules(Module) ->
    case application:get_application(Module) of
        undefined -> [];
        {ok, App} ->
            Modules = application:get_env(App, config_verification_modules, []),
            lists:usort(Modules)
    end.

-spec is_exported([module()],[function()], atom(), non_neg_integer()) ->
    function() | not_exported | {multiple_functions_found, [function()]}.
is_exported([],[], _, _) ->
    not_exported;
is_exported([],[FN], _, _) ->
    FN;
is_exported([],Functions, _, _)when length(Functions)>1 ->
    {multiple_functions_found, Functions};
is_exported([Module | T], Functions, FunctionName, Arity) ->
    case erlang:function_exported(Module, FunctionName, Arity) of
        true ->
            FN=fun Module:FunctionName/Arity,
            is_exported(T,[FN|Functions],FunctionName, Arity);
        false ->
            is_exported(T, Functions, FunctionName, Arity)
    end.

-spec one_of_fun(one_of()) -> verification_fun().
one_of_fun(OneOf) ->
    fun(X) ->
        case lists:member(X, OneOf) of
            true -> true;
            false -> {false, {not_one_of, OneOf}}
        end
    end.
