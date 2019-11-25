%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config).

-export([get/1, get/2]).

-export_type([name/0, value/0]).

-type name() :: atom().
-type value() :: any().

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-spec get(name()) -> any().
get(Name) ->
    get(Name, undefined).

-spec get(name(), value()) -> value().
get(Name, Default) when is_atom(Name) ->
    case ets:lookup(amoc_config, Name) of
        [] ->
            ?LOG_ERROR("no scenario setting ~p", [Name]),
            Default;
        [{Name, undefined}] ->
            Default;
        [{Name, Value}] ->
            Value;
        InvalidLookupRet ->
            ?LOG_ERROR("invalid lookup return value ~p ~p", [Name, InvalidLookupRet]),
            throw({invalid_lookup_ret_value, InvalidLookupRet})
    end.
