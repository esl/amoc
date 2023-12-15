%% @private
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_telemetry).

-export([execute/3, execute_log/4]).

-spec execute(EventName, Measurements, Metadata) -> ok when
      EventName :: telemetry:event_name(),
      Measurements :: telemetry:event_measurements(),
      Metadata :: telemetry:event_metadata().
execute(Name, Measurements, Metadata) ->
    TimeStamp = erlang:monotonic_time(),
    PrefixedName = [amoc | Name],
    MetadataWithTS = Metadata#{monotonic_time => TimeStamp},
    telemetry:execute(PrefixedName, Measurements, MetadataWithTS).

-spec execute_log(Level, EventName, Metadata, Msg) -> ok when
      Level :: logger:level(),
      EventName :: telemetry:event_name(),
      Metadata :: telemetry:event_metadata(),
      Msg :: binary().
execute_log(Level, Name, Metadata, Message) ->
    MetadataWithLog = Metadata#{log_level => Level, msg => Message},
    execute(Name, #{Level => 1}, MetadataWithLog).
