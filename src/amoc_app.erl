%% @private
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    amoc_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
