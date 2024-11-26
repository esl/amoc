%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
-module(amoc_throttle_config).

-define(TIMEOUT(N), (infinity =:= N orelse is_integer(N) andalso N >= 0)).
-define(NON_NEG_INT(N), (is_integer(N) andalso N >= 0)).
-define(POS_INT(N), (is_integer(N) andalso N > 0)).
-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(DEFAULT_STEP_SIZE, 1).

-export([verify_config/1, verify_gradual_config/1, pool_config/2, process_pool_config/2]).
-export_type([config/0, gradual/0, pool_config/0]).

-type process_number() :: non_neg_integer().
-type config() :: #{rate := amoc_throttle:rate(),
                    interval := amoc_throttle:interval()}.
-type gradual() :: #{from_rate := non_neg_integer(),
                     to_rate := non_neg_integer(),
                     interval := amoc_throttle:interval(),
                     step_interval := pos_integer(),
                     step_size := pos_integer(),
                     step_count := pos_integer()}.
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

-spec verify_gradual_config(amoc_throttle:gradual_rate_config()) ->
    gradual() | {error, any()}.
verify_gradual_config(Config) ->
    try do_verify_gradual_config(Config) of
        Change -> Change
    catch error:Reason ->
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
    Processes = supervisor:which_children(PoolSup),
    Workers = [ {N, Pid} || {{amoc_throttle_process, N}, Pid, _, _} <- Processes, is_pid(Pid) ],
    Fun1 = fun(N, Config) ->
                   {_, Pid} = lists:keyfind(N, 1, Workers),
                   Config#{pid => Pid}
           end,
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

-spec do_verify_gradual_config(amoc_throttle:gradual_rate_config()) -> gradual().
do_verify_gradual_config(
  #{from_interarrival := FromInterarrival, to_interarrival := ToInterarrival} = Config0)
  when ?POS_INT(FromInterarrival), ?POS_INT(ToInterarrival), not is_map_key(interval, Config0) ->
    FromRate = ?DEFAULT_INTERVAL div FromInterarrival,
    ToRate = ?DEFAULT_INTERVAL div ToInterarrival,
    Config1 = Config0#{from_rate => FromRate, to_rate => ToRate},
    do_verify_gradual_config(maps:without([from_interarrival, to_interarrival], Config1));
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate,
    step_interval := StepInterval, step_count := StepCount, step_size := StepSize} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    check_step_parameters((ToRate - FromRate) div StepCount, StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, duration := Duration}) ->
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs(Duration div (ToRate - FromRate)),
    #{from_rate => FromRate, to_rate => ToRate, interval => ?DEFAULT_INTERVAL,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate,
    step_interval := StepInterval, step_size := StepSize} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    StepCount = abs((ToRate - FromRate) div StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, step_interval := StepInterval} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepCount = abs((ToRate - FromRate) div StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, step_count := StepCount} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepInterval = (ToRate - FromRate) div (StepSize * StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, step_size := StepSize} = Config) ->
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs((ToRate - FromRate) div StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate} = Config) ->
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs((ToRate - FromRate) div StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize}.

check_step_size_with_from_to_rate(From, To, StepSize) when From =< To, StepSize >= 0 -> ok;
check_step_size_with_from_to_rate(From, To, StepSize) when From > To, StepSize < 0 -> ok.

step_size_sign(From, To) when From =< To -> 1;
step_size_sign(From, To) when From > To -> -1.

check_step_parameters(StepSize, StepSize) -> ok.
