-module(iq_metrics).

-export([start/1, request/2, request/3, timeout/2, response/2, new_id/1]).

-record(request, {
    key :: {Id :: binary(), pid()},
    name :: name(),
    time :: integer(),
    tref :: timer:tref() | undefined | timed_out
}).

-type name() :: atom().

-define(TABLE, request).

-spec start([name()]) -> ok.
start(Names) ->
    {ok, App} = application:get_application(),
    PrivDir = code:priv_dir(App),
    application:set_env(mnesia, dir, PrivDir),

    mnesia:create_schema([node()]),
    application:ensure_started(mnesia),
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, request)}]),

    Suffixes = [[request], [response], [timeout], [response, result], [response, error], [timeout, result], [timeout, error]],
    [begin
         [amoc_metrics:init(counters, [Name | Suffix]) || Suffix <- Suffixes],
         amoc_metrics:init(times, [Name, response, time])
     end || Name <- Names],
    ok.

%% API

-spec request(name(), binary()) -> any().
request(Name, Id) ->
    process_request(Name, Id, undefined).

-spec request(name(), binary(), timeout()) -> any().
request(Name, Id, Timeout) ->
    {ok, TRef} = timer:apply_after(Timeout, ?MODULE, timeout, [{Id, self()}, no_delete]),
    process_request(Name, Id, TRef).

-spec timeout({binary(), pid()}, delete | no_delete) -> any();
             (binary(), delete | no_delete) -> any().
timeout({_Id, _Pid} = Key, no_delete) ->
    F = fun() ->
            case mnesia:read(?TABLE, Key) of
                [Req] -> mnesia:write(Req#request{tref = timed_out}),
                         Req;
                [] -> ok % timed out during success or failure execution
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {atomic, Req} -> Name = Req#request.name,
                         amoc_metrics:update_counter([Name, timeout])
    end;
timeout({_Id, _Pid} = Key, delete) ->
    F = fun() ->
            case mnesia:read(?TABLE, Key) of
                [Req] -> mnesia:delete({?TABLE, Key}),
                         Req;
                [] -> ok % timed out during success or failure execution
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {atomic, Req} -> Name = Req#request.name,
                         amoc_metrics:update_counter([Name, timeout])
    end;
timeout(Id, Flag) when is_binary(Id) ->
    timeout({Id, self()}, Flag).

-spec response(Id :: binary(), result | error) -> any().
response(Id, Type) ->
    Now = os:system_time(microsecond),
    Key = {Id, self()},
    F = fun() ->
            [Req] = mnesia:read(?TABLE, Key),
            mnesia:delete({?TABLE, Key}),
            Req
        end,
    {atomic, Req} = mnesia:transaction(F),
    Name = Req#request.name,
    amoc_metrics:update_counter([Name, response]),
    ReqTime = Req#request.time,
    amoc_metrics:update_time([Name, response, time], Now - ReqTime),
    case Req#request.tref of
        timed_out -> amoc_metrics:update_counter([Name, timeout, Type]);
        undefined -> amoc_metrics:update_counter([Name, response, Type]);
        TRef -> amoc_metrics:update_counter([Name, response, Type]),
                timer:cancel(TRef)
    end.

-spec new_id(binary()) -> binary().
new_id(Prefix) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, "-", Id/binary>>.

%% Internal functions

process_request(Name, Id, TRef) ->
    Now = os:system_time(microsecond),
    Req = #request{key = {Id, self()}, name = Name, time = Now, tref = TRef},
    F = fun() -> mnesia:write(Req) end,
    {atomic, _} = mnesia:transaction(F),
    amoc_metrics:update_counter([Name, request]).
