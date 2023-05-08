%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_config).

-include_lib("kernel/include/logger.hrl").
-include("amoc_config.hrl").

-export([get/1, get/2]).
-export_type([name/0, value/0, settings/0, maybe_module_config/0]).

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
            throw({invalid_setting, Name});
        [#module_parameter{name = Name, value = undefined}] ->
            Default;
        [#module_parameter{name = Name, value = Value}] ->
            Value;
        InvalidLookupRet ->
            ?LOG_ERROR("invalid lookup return value ~p ~p", [Name, InvalidLookupRet]),
            throw({invalid_lookup_ret_value, InvalidLookupRet})
    end.
