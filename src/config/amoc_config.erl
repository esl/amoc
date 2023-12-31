%% @see amoc_config
%% @copyright 2023 Erlang Solutions Ltd.
%% @doc TODO
-module(amoc_config).

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
            amoc_telemetry:execute_log(
              error, [config, get], #{setting => Name}, <<"no scenario setting">>),
            throw({invalid_setting, Name});
        [#module_parameter{name = Name, value = undefined}] ->
            Default;
        [#module_parameter{name = Name, value = Value}] ->
            Value;
        InvalidLookupRet ->
            amoc_telemetry:execute_log(
              error, [config, get], #{setting => Name, return => InvalidLookupRet},
              <<"invalid lookup return value">>),
            throw({invalid_lookup_ret_value, InvalidLookupRet})
    end.
