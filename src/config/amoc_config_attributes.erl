%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
%% this module is responsible for parsing module attributes
%%==============================================================================
-module(amoc_config_attributes).

-include("amoc_config.hrl").

%% API
-export([get_module_configuration/2]).
-export([none/1, none/2]).
-ifdef(TEST).
-export([%% exported for testing only
         get_module_attributes/2,
         process_module_attributes/2]).
-endif.

-type maybe_module_attribute() :: module_attribute() | term().
-type maybe_verification_method() :: verification_method() | term().
-type maybe_update_method() :: update_method() | term().

-type attribute_name() :: required_variable | override_variable.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get_module_configuration(attribute_name(), module()) ->
    maybe_module_config().
get_module_configuration(AttrName, Module) ->
    ScenarioAttributes = get_module_attributes(AttrName, Module),
    process_module_attributes(Module, ScenarioAttributes).

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

-spec process_module_attributes(module(), [maybe_module_attribute()]) ->
    maybe_module_config().
process_module_attributes(Module, ScenarioAttributes) ->
    Config = [process_var_attr(Module, Attr)
              || Attr <- ScenarioAttributes],
    amoc_config_utils:maybe_error(invalid_attribute_format, Config).

-spec process_var_attr(module(), maybe_module_attribute()) ->
    {ok, module_parameter()} | {error, reason()}.
process_var_attr(Module, Attr) ->
    PipelineActions = [
        {fun check_mandatory_fields/1, []},
        {fun check_default_value/1, []},
        {fun check_verification_method/1, []},
        {fun check_update_method/1, []},
        {fun make_module_parameter/2, [Module]}],
    case amoc_config_utils:pipeline(PipelineActions, {ok, Attr}) of
        {error, Reason} -> {error, add_original_attribute(Reason, Attr)};
        {ok, Param} -> {ok, Param}
    end.

-spec check_mandatory_fields(maybe_module_attribute()) ->
    {ok, #{name := name(), description := string(), any() => any()}} | {error, reason()}.
check_mandatory_fields(#{description := List, name := Atom} = Attr) when is_atom(Atom),
                                                                         is_list(List) ->
    case io_lib:char_list(List) of
        true -> {ok, Attr};
        false -> {error, invalid_attribute}
    end;
check_mandatory_fields(_Attr) ->
    {error, invalid_attribute}.

-spec check_default_value(#{any() => any()}) ->
    {ok, #{default_value := value(), any() => any()}}.
check_default_value(Attr) ->
    DefaultValue = maps:get(default_value, Attr, undefined),
    {ok, Attr#{default_value => DefaultValue}}.

-spec check_verification_method(#{any() => any()}) ->
    {ok, #{verification := maybe_verification_fun(), any() => any()}} | {error, reason()}.
check_verification_method(Attr) ->
    VerificationMethod = maps:get(verification, Attr, none),
    case verification_fn(VerificationMethod) of
        not_exported ->
            {error, verification_method_not_exported};
        invalid_method ->
            {error, invalid_verification_method};
        VerificationFn ->
            {ok, Attr#{verification => VerificationFn}}
    end.

-spec check_update_method(#{any() => any()}) ->
    {ok, #{update := maybe_update_fun(), any() => any()}} | {error, reason()}.
check_update_method(Attr) ->
    UpdateMethod = maps:get(update, Attr, read_only),
    case update_fn(UpdateMethod) of
        not_exported ->
            {error, update_method_not_exported};
        invalid_method ->
            {error, invalid_update_method};
        UpdateFn ->
            {ok, Attr#{update => UpdateFn}}
    end.

-spec make_module_parameter(#{name := name(),
                              description := string(),
                              default_value := value(),
                              verification := maybe_verification_fun(),
                              update := maybe_update_fun(),
                              any() => any()},
                            module()) ->
    {ok, module_parameter()}.
make_module_parameter(#{name := Name, description := Description, default_value := Value,
                        verification := VerificationFn, update := UpdateFn}, Module) ->
    {ok, #module_parameter{name = Name, mod = Module, description = Description, value = Value,
                           verification_fn = VerificationFn, update_fn = UpdateFn}}.

-spec verification_fn(maybe_verification_method()) ->
    maybe_verification_fun() | not_exported | invalid_method.
verification_fn(none) ->
    fun ?MODULE:none/1;
verification_fn([_ | _] = OneOf) ->
    one_of_fun(OneOf);
verification_fn(Fun) when is_function(Fun, 1) ->
    is_exported(Fun);
verification_fn({Module, Function, 1}) ->
    is_exported(fun Module:Function/1);
verification_fn(_) ->
    invalid_method.

-spec update_fn(maybe_update_method()) ->
    maybe_update_fun() | not_exported | invalid_method | read_only.
update_fn(read_only) ->
    read_only;
update_fn(none) ->
    fun ?MODULE:none/2;
update_fn(Fun) when is_function(Fun, 2) ->
    is_exported(Fun);
update_fn({Module, Function, 2}) ->
    is_exported(fun Module:Function/2);
update_fn(_) ->
    invalid_method.

-spec is_exported(function()) ->
    function() | not_exported.
is_exported(Fn) ->
    [{type, T}, {module, M}, {name, F}, {arity, A}] =
        [erlang:fun_info(Fn, I) || I <- [type, module, name, arity]],
    case {T, code:ensure_loaded(M)} of
        {external, {module, M}} ->
            case erlang:function_exported(M, F, A) of
                true -> Fn;
                false -> not_exported
            end;
        _ -> not_exported
    end.

-spec one_of_fun(one_of()) -> verification_fun().
one_of_fun(OneOf) ->
    fun(X) ->
        case lists:member(X, OneOf) of
            true -> true;
            false -> {false, {not_one_of, OneOf}}
        end
    end.

add_original_attribute(Reason, Attr) when is_tuple(Reason) ->
    list_to_tuple([Attr | tuple_to_list(Reason)]);
add_original_attribute(Reason, Attr) ->
    {Attr, Reason}.
