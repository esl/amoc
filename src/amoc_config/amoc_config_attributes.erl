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
    AllVerificationModules = [Module | VerificationModules],
    PipelineActions = [
        {fun check_mandatory_fields/1, []},
        {fun check_default_value/1, []},
        {fun check_verification_method/3, [AllVerificationModules,Attr]},
        {fun check_update_method/3, [AllVerificationModules,Attr]},
        {fun make_module_parameter/2, [Module]}],
    amoc_config_utils:pipeline(PipelineActions, {ok, Attr}).

check_mandatory_fields(#{description := List, name := Atom} = Attr) when is_atom(Atom),
                                                                         is_list(List) ->
    case io_lib:char_list(List) of
        true -> {ok, Attr};
        false -> {error, {invalid_attribute, Attr}}
    end;
check_mandatory_fields(Attr) ->
    {error, {invalid_attribute, Attr}}.

check_default_value(Attr) ->
    DefaultValue = maps:get(value, Attr, undefined),
    {ok, Attr#{value => DefaultValue}}.

check_verification_method(Attr, VerificationModules, OriginalAttr) ->
    VerificationMethod = maps:get(verification, Attr, none),
    case verification_fn(VerificationModules, VerificationMethod) of
        not_exported ->
            {error, {verification_method_not_exported, OriginalAttr,
                     VerificationModules}};
        invalid_method ->
            {error, {invalid_verification_method, OriginalAttr}};
        VerificationFn ->
            {ok, Attr#{verification => VerificationFn}}
    end.

check_update_method(Attr, VerificationModules, OriginalAttr) ->
    UpdateMethod = maps:get(update, Attr, read_only),
    case update_fn(VerificationModules, UpdateMethod) of
        not_exported ->
            {error, {update_method_not_exported, OriginalAttr,
                     VerificationModules}};
        invalid_method ->
            {error, {invalid_update_method, OriginalAttr}};
        UpdateFn ->
            {ok, Attr#{update=>UpdateFn}}
    end.

make_module_parameter(#{name := Name, value := Value, update := UpdateFn,
                        verification := VerificationFn}, Module) ->
    {ok, #module_parameter{name = Name, mod = Module, value = Value,
                           verification_fn = VerificationFn,update_fn = UpdateFn}}.

-spec verification_fn([module()], maybe_verification_method()) ->
    maybe_verification_fun() | not_exported | invalid_method.
verification_fn(_, none) ->
    fun none/1;
verification_fn(_, [_ | _] = OneOF) ->
    one_of_fun(OneOF);
verification_fn(_, Fun) when is_function(Fun, 1) ->
    Fun;
verification_fn(Modules, Atom) when is_atom(Atom) ->
    is_exported(Modules, Atom, 1);
verification_fn(_, _) ->
    invalid_method.

-spec update_fn([module()], maybe_update_method()) ->
    maybe_update_fun() | not_exported | invalid_method | read_only.
update_fn(_, read_only) ->
    read_only;
update_fn(_, none) ->
    fun none/2;
update_fn(_, Fun) when is_function(Fun, 2) ->
    Fun;
update_fn(Modules, Atom) when is_atom(Atom) ->
    is_exported(Modules, Atom, 2);
update_fn(_, _) ->
    invalid_method.

-spec is_exported([module()], atom(), non_neg_integer()) ->
    function() | not_exported.
is_exported([], _, _) ->
    not_exported;
is_exported([Module | T], Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true -> fun Module:Function/Arity;
        false -> is_exported(T, Function, Arity)
    end.

none(_) -> true.



none(_, _) -> ok.

-spec one_of_fun(one_of()) -> verification_fun().
one_of_fun(OneOf) ->
    fun(X) ->
        case lists:member(X, OneOf) of
            true -> true;
            false -> {false, {not_one_of, OneOf}}
        end
    end.
