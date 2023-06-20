%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================


%%------------------------------------------------------------------------------
%% basic types
%%------------------------------------------------------------------------------
-type name() :: atom().
-type value() :: any().

%%------------------------------------------------------------------------------
%% runtime supplied settings
%%------------------------------------------------------------------------------
-type settings() :: [{name(), value()}].

%%------------------------------------------------------------------------------
%% error types
%%------------------------------------------------------------------------------
-type reason() :: any().
-type error_type() :: atom().
-type error() :: {error, error_type(), reason()}.

%%------------------------------------------------------------------------------
%% module attributes definition
%%------------------------------------------------------------------------------
-type verification_fun() :: fun((Value :: value()) -> boolean() |
                                                      {true, NewValue :: value()} |
                                                      {false, reason()}).

-type update_fun() :: fun((ParamName :: name(), NewValue :: value()) -> any()).

-type maybe_verification_fun() :: verification_fun() | fun((_)-> any()).
-type maybe_update_fun() :: update_fun() | fun((_,_)-> any()).

-record(module_parameter, {name :: name(),
                           mod :: module(),
                           value :: value(),
                           description :: string(),
                           verification_fn :: maybe_verification_fun(),
                           update_fn = read_only :: maybe_update_fun() | read_only}).

-type module_parameter() :: #module_parameter{}.
-type module_configuration() :: [module_parameter()].
-type maybe_module_config() :: {ok, [module_parameter()]} | error().

-type one_of() :: [value(), ...].
-type verification_method() :: none | one_of() | verification_fun().
-type update_method() :: read_only | none | update_fun().

-type module_attribute() :: #{ name := name(),
                               description := string(),
                               default_value => value(),
                               verification => verification_method(),
                               update => update_method()}.
