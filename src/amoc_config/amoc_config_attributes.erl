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
-type raw_module_parameter() ::
    {amoc_config:name(), module(), amoc_config:value(), maybe_verification_method()}.

-type attribute_name() :: required_variable | override_variable.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get_module_configuration(attribute_name(), module(), [module()]) ->
    {ok, module_configuration()} | amoc_config_utils:error().
get_module_configuration(AttrName, Module, VerificationModules) ->
    try
        ScenarioAttributes = get_module_attributes(AttrName, Module),
        AllVerificationModules = [Module | VerificationModules],
        process_module_attributes(AllVerificationModules, Module, ScenarioAttributes)
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
process_var_attr(_, Module, Attr) when not is_atom(element(1, Attr));
                                       not is_list(element(2, Attr)) ->
    {error, {invalid_attribute, Attr}};
process_var_attr(_, Module, {Name, _}) ->
    {ok, {Name, Module, undefined, fun none/1}};
process_var_attr(_, Module, {Name, _, DefaultValue}) ->
    {ok, {Name, Module, DefaultValue, fun none/1}};
process_var_attr(VerificationModules, Module, {Name, _, DefaultValue,
                                               VerificationMethod}) ->
    Parameter = {Name, Module, DefaultValue, VerificationMethod},
    check_verification_method(VerificationModules, Parameter);
process_var_attr(_, _, InvalidAttribute) ->
    {error, {invalid_attribute, InvalidAttribute}}.

-spec check_verification_method([module()], raw_module_parameter()) ->
    {ok, module_parameter()} | {error, reason()}.
check_verification_method(Modules, {Name, Module, DefaultValue,
                                    VerificationMethod} = Param) ->
    case verification_fun(Modules, VerificationMethod) of
        not_exported -> {error, {verification_method_not_exported, Param, Modules}};
        invalid_method -> {error, {invalid_verification_method, Param}};
        VerificationFn -> {ok, {Name, Module, DefaultValue, VerificationFn}}
    end.

-spec verification_fun([module()], maybe_verification_method()) ->
    verification_fun() | not_exported | invalid_method.
verification_fun(_, none) ->
    fun none/1;
verification_fun(_, [_ | _] = OneOF) ->
    one_of_fun(OneOF);
verification_fun(_, Fun) when is_function(Fun, 1) ->
    Fun;
verification_fun(Modules, Atom) when is_atom(Atom) ->
    is_exported(Modules, Atom, 1);
verification_fun(_, _) ->
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

-spec one_of_fun(one_of()) -> verification_fun().
one_of_fun(OneOf) ->
    fun(X) ->
        case lists:member(X, OneOf) of
            true -> true;
            false -> {false, {not_one_of, OneOf}}
        end
    end.
