%% @private
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_telemetry).

-export([execute/3]).

-spec execute(EventName, Measurements, Metadata) -> ok when
      EventName :: telemetry:event_name(),
      Measurements :: telemetry:event_measurements(),
      Metadata :: telemetry:event_metadata().
execute(Name, Measurements, Metadata) ->
    TimeStamp = erlang:monotonic_time(),
    NameWithAmocPrefix = [amoc | Name],
    MetadataWithTS = Metadata#{monotonic_time => TimeStamp},
    telemetry:execute(NameWithAmocPrefix, Measurements, MetadataWithTS).
