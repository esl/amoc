-module(mongoose_push_api_load).

-behavior(amoc_scenario).

-export([start/1]).
-export([init/0]).

-define(HOST, "127.0.0.1").
-define(PORT, 8443).
-define(PROTOCOL_VERSION, "v1").

-define(SERVICES, [fcm, apns]).

-define(NOTIFICATIONS_ALL(S),          [notifications, S, all]).
-define(NOTIFICATIONS_SENT(S),         [notifications, S, sent]).
-define(NOTIFICATIONS_OK(S),           [notifications, S, ok]).
-define(NOTIFICATIONS_ERROR(S),        [notifications, S, error]).
-define(NOTIFICATIONS_PUSH_TIME(S),    [notifications, S, push_time]).

-define(SPIRAL_METRICS(S), [
    ?NOTIFICATIONS_ALL(S), ?NOTIFICATIONS_SENT(S), ?NOTIFICATIONS_OK(S), ?NOTIFICATIONS_ERROR(S)
]).

-spec init() -> ok.
init() ->
    http_req:start(),

    lists:foreach(
        fun(Service) ->
            lists:foreach(
                fun(Metric) ->
                        amoc_metrics:init(counters, Metric)
                end, ?SPIRAL_METRICS(Service)),

            amoc_metrics:init(times, ?NOTIFICATIONS_PUSH_TIME(Service))
        end, ?SERVICES),

    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    DeviceId = device_id(MyId),
    Path = <<"/", ?PROTOCOL_VERSION, "/notification/", DeviceId/binary>>,
    Notification = #{service := Service} = gen_notification(MyId),
    Headers =
        [
            {<<"content-type">>, <<"application/json">>},
            {<<"accept">>, <<"application/json">>}
        ],
    URL = "https://" ++ ?HOST ++ ":" ++ integer_to_list(?PORT),
    lager:debug("Request to: ~p", [URL]),

    amoc_metrics:update_counter(?NOTIFICATIONS_ALL(Service), 1),
    case http_req:post_request(URL, Path, Headers, jiffy:encode(Notification)) of
        {ok, {{StatusCodeBin, StateMsg}, _RespHeaders, RespBody, _, TimeDiff}} ->
            amoc_metrics:update_counter(?NOTIFICATIONS_SENT(Service), 1),
            amoc_metrics:update_time(?NOTIFICATIONS_PUSH_TIME(Service), TimeDiff),
            case binary_to_integer(StatusCodeBin) of
                StatusCode when StatusCode >= 200, StatusCode < 300 ->
                    amoc_metrics:update_counter(?NOTIFICATIONS_OK(Service), 1);
                _StatusCode ->
                    amoc_metrics:update_counter(?NOTIFICATIONS_ERROR(Service), 1)
            end,

            lager:debug("Request finished ~p: ~p ~p ~p ~p ms",
                        [Path, StatusCodeBin, StateMsg, RespBody, TimeDiff / 1000]);
        {error, Reason} ->
            lager:info("Request error ~p", [Reason])
    end,
    ok.


device_id(MyId) ->
    Hash = crypto:hash(md5, integer_to_binary(MyId)),
    Num = binary:decode_unsigned(Hash),
    integer_to_binary(Num, 16).

gen_notification(MyId) ->
    #{service => lists:nth(MyId rem length(?SERVICES) + 1, ?SERVICES),
      body => base64:encode(crypto:strong_rand_bytes(50)),
      title => base64:encode(crypto:strong_rand_bytes(10))
    }.
