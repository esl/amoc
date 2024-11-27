%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_throttle_config).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT(N), (infinity =:= N orelse is_integer(N) andalso N >= 0)).
-define(NON_NEG_INT(N), (is_integer(N) andalso N >= 0)).
-define(POS_INT(N), (is_integer(N) andalso N > 0)).
-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(DEFAULT_STEP_INTERVAL, 100). %% every 100ms

-export([verify_config/1, verify_gradual_config/1, pool_config/2, process_pool_config/2]).
-export_type([config/0, gradual_plan/0, pool_config/0]).

-type process_number() :: non_neg_integer().
-type config() :: #{rate := amoc_throttle:rate(),
                    interval := amoc_throttle:interval()}.
-type gradual_plan() :: #{rates := [non_neg_integer()],
                          interval := amoc_throttle:interval(),
                          step_interval := non_neg_integer()}.
-type pool_config() :: #{process_number() :=
                         #{max_n := infinity | non_neg_integer(),
                           delay := non_neg_integer(),
                           status := active | inactive,
                           pid := undefined | pid()}}.

-spec verify_config(amoc_throttle:t()) -> config() | {error, any()}.
verify_config(#{interarrival := infinity} = Config)
  when 1 =:= map_size(Config) ->
    #{rate => 0, interval => ?DEFAULT_INTERVAL};
verify_config(#{interarrival := 0} = Config)
  when 1 =:= map_size(Config) ->
    #{rate => infinity, interval => ?DEFAULT_INTERVAL};
verify_config(#{interarrival := Interarrival} = Config)
  when 1 =:= map_size(Config), ?POS_INT(Interarrival) ->
    #{rate => ?DEFAULT_INTERVAL div Interarrival, interval => ?DEFAULT_INTERVAL};
verify_config(#{rate := Rate, interval := Interval} = Config)
  when 2 =:= map_size(Config), ?TIMEOUT(Rate), ?NON_NEG_INT(Interval) ->
    Config;
verify_config(#{rate := Rate} = Config)
  when 1 =:= map_size(Config), ?TIMEOUT(Rate) ->
    Config#{interval => ?DEFAULT_INTERVAL};
verify_config(_Config) ->
    {error, invalid_throttle}.

-spec verify_gradual_config(amoc_throttle:gradual_plan()) -> gradual_plan() | {error, any()}.
verify_gradual_config(Config) ->
    try do_verify_gradual_config(Config) of
        Change -> Change
    catch error:Reason:Stacktrace ->
              ?LOG_WARNING(#{what => bad_gradual_config,
                            reason => Reason, stacktrace => Stacktrace}),
              {error, Reason}
    end.

-spec pool_config(amoc_throttle:rate(), amoc_throttle:interval()) -> pool_config().
pool_config(infinity, _) ->
    Config = #{max_n => infinity, delay => 0, status => active, pid => undefined},
    maps:from_keys(lists:seq(1, no_of_processes()), Config);
pool_config(0, _) ->
    Config = #{max_n => 0, delay => infinity, status => active, pid => undefined},
    maps:from_keys(lists:seq(1, no_of_processes()), Config);
pool_config(Rate, 0) ->
    Config = #{max_n => Rate, delay => 0, status => inactive, pid => undefined},
    PoolConfig = #{1 := First} = maps:from_keys(lists:seq(1, no_of_processes()), Config),
    PoolConfig#{1 := First#{status => active}};
pool_config(Rate, Interval) ->
    NoOfProcesses = no_of_processes(),
    RatePerMinutePerProcess = (60000 * Rate div Interval) div NoOfProcesses,
    DelayPerProcess = (NoOfProcesses * Interval) div Rate,
    Rem = ((60000 * Rate div Interval) rem NoOfProcesses)
            + ((NoOfProcesses * Interval) rem Rate),
    calculate_availability(RatePerMinutePerProcess, DelayPerProcess, NoOfProcesses, Rem).

-spec process_pool_config(pid(), pool_config()) -> pool_config().
process_pool_config(PoolSup, PoolConfig) ->
    Workers = amoc_throttle_pool:get_workers(PoolSup),
    Fun1 = fun(N, Config) -> Config#{pid => maps:get(N, Workers)} end,
    maps:map(Fun1, PoolConfig).

-spec calculate_availability(integer(), integer(), pos_integer(), integer()) -> pool_config().
calculate_availability(RatePerMinutePerProcess, DelayPerProcess, NoOfProcesses, Rem) ->
    Fun = fun(N, {Acc, R}) ->
                  case {RatePerMinutePerProcess < NoOfProcesses, R} of
                      {true, 0} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess + 1,
                                     status => inactive, pid => undefined},
                          {Acc#{N => Config}, R};
                      {true, R} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess,
                                     status => active, pid => undefined},
                          {Acc#{N => Config}, R - 1};
                      {false, 0} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess,
                                     status => active, pid => undefined},
                          {Acc#{N => Config}, R};
                      {false, R} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess + 1,
                                     status => active, pid => undefined},
                          {Acc#{N => Config}, R - 1}
                  end
          end,
    {#{} = PoolConfig, _} = lists:foldl(Fun, {#{}, Rem}, lists:seq(1, NoOfProcesses)),
    PoolConfig.

-spec no_of_processes() -> non_neg_integer().
no_of_processes() ->
    3 * erlang:system_info(schedulers_online).

-spec do_verify_gradual_config(amoc_throttle:gradual_plan()) -> gradual_plan().
do_verify_gradual_config(
  #{throttle := #{from_rate := FromRate, to_rate := ToRate, interval := Interval} = Throttle,
    plan := #{step_interval := StepInterval, step_count := StepCount} = Plan})
  when 3 =:= map_size(Throttle), 2 =:= map_size(Plan),
       ?NON_NEG_INT(FromRate), ?NON_NEG_INT(ToRate), ?NON_NEG_INT(Interval),
       ?POS_INT(StepInterval), ?POS_INT(StepCount) ->
    StepRate = (ToRate - FromRate) / StepCount,
    StepPlan = [ calculate_step(Step, StepRate, FromRate) || Step <- lists:seq(0, StepCount) ],
    #{rates => StepPlan, interval => Interval, step_interval => StepInterval};

do_verify_gradual_config(
  #{throttle := #{from_rate := _, to_rate := _} = Throttle} = Config0)
  when 2 =:= map_size(Throttle) ->
    Config1 = Config0#{throttle := Throttle#{interval => ?DEFAULT_INTERVAL}},
    do_verify_gradual_config(Config1);

do_verify_gradual_config(
  #{throttle := #{from_interarrival := FromInterarrival,
                  to_interarrival := ToInterarrival} = Throttle} = Config0)
  when ?NON_NEG_INT(FromInterarrival), ?NON_NEG_INT(ToInterarrival), 2 =:= map_size(Throttle) ->
    FromRate = ?DEFAULT_INTERVAL div FromInterarrival,
    ToRate = ?DEFAULT_INTERVAL div ToInterarrival,
    Config1 = Config0#{throttle := #{from_rate => FromRate, to_rate => ToRate}},
    do_verify_gradual_config(Config1);

do_verify_gradual_config(
  #{throttle := #{from_rate := FromRate, to_rate := ToRate, interval := Interval} = Throttle,
    plan := #{duration := Duration} = Plan} = Config0)
  when 3 =:= map_size(Throttle), 1 =:= map_size(Plan),
       ?NON_NEG_INT(FromRate), ?NON_NEG_INT(ToRate), ?NON_NEG_INT(Interval), ?POS_INT(Duration) ->
    StepCount = abs(Duration div ?DEFAULT_STEP_INTERVAL),
    Config1 = Config0#{plan := #{step_interval => ?DEFAULT_STEP_INTERVAL, step_count => StepCount}},
    do_verify_gradual_config(Config1).

-spec calculate_step(
        Step :: non_neg_integer(),
        StepRate :: float(),
        FromRate :: non_neg_integer()) ->
    non_neg_integer().
calculate_step(N, StepRate, From) ->
    From + floor(StepRate * N).
