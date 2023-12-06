-module(async_helper).

-export([wait_until/2, wait_until/3]).

% @doc Waits `TimeLeft` for `Fun` to return `Expected Value`, then returns `ExpectedValue`
% If no value is returned or the result doesn't match  `ExpectedValue` error is raised
wait_until(Fun, ExpectedValue) ->
    wait_until(Fun, ExpectedValue, #{}).

%% Example: wait_until(fun () -> ... end, SomeVal, #{time_left => timer:seconds(2)})
%% if expected value is a function with arity 1, it's treated as a validation function.
wait_until(Fun, ValidatorFn, Opts0) when is_function(ValidatorFn, 1) ->
    Defaults = #{time_left => timer:seconds(5), sleep_time => 50},
    Opts1 = maps:merge(Defaults, Opts0),
    Opts = Opts1#{validator => ValidatorFn, history => []},
    do_wait_until(Fun, Opts);
wait_until(Fun, ExpectedValue, Opts) ->
    ValidatorFn = fun(Value) -> Value =:= ExpectedValue end,
    wait_until(Fun, ValidatorFn, Opts).

do_wait_until(_Fun, #{time_left := TimeLeft, history := History}) when TimeLeft =< 0 ->
    error({badmatch, lists:reverse(History)});

do_wait_until(Fun, #{validator := Validator} = Opts) ->
    try Fun() of
        Value -> case Validator(Value) of
                   true -> {ok, Value};
                   _ -> wait_and_continue(Fun, Value, Opts)
               end
    catch Error:Reason:Stacktrace ->
            wait_and_continue(Fun, {Error, Reason, Stacktrace}, Opts)
    end.

wait_and_continue(Fun, FunResult, #{time_left := TimeLeft,
                                    sleep_time := SleepTime,
                                    history := History} = Opts) ->
    timer:sleep(SleepTime),
    do_wait_until(Fun, Opts#{time_left => TimeLeft - SleepTime,
                             history => [FunResult | History]}).
