%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
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
-type parameter() :: {name(), value()}.
-type settings() :: [parameter()].

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

-type module_parameter() :: {ParamName :: name(), module(), DefValue :: value(),
                             verification_fun()}.

-type module_configuration() :: [module_parameter()].

-type one_of() :: [value(), ...].
-type fn_name() :: atom().
-type verification_method() :: none | one_of() | fn_name() | verification_fun().

-type module_attribute() ::
    {ParamName :: name(), Description :: string()} |
    {ParamName :: name(), Description :: string(), DefValue :: value()} |
    {ParamName :: name(), Description :: string(), DefValue :: value(),
                          verification_method()}.




