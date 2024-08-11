Amoc also exposes the following telemetry events:

## Scenario

All telemetry spans below contain an extra key `return` in the metadata for the `stop` event with the return value of the given callback.

A telemetry span of a scenario initialisation (i.e. the exported `init/0` function):

```erlang
event_name: [amoc, scenario, init, _]
measurements: #{}                 %% As described in `telemetry:span/3`
metadata: #{scenario := module()} %% Plus as described in `telemetry:span/3`
```

A telemetry span of a full scenario execution for a user (i.e. the exported `start/1,2` function):

```erlang
event_name: [amoc, scenario, start, _]
measurements: #{}                        %% As described in `telemetry:span/3`
metadata: #{scenario := module(),        %% Running scenario
            state := term(),             %% The state as returned by `init/0`
            user_id := non_neg_integer() %% User ID assigned to the running process
           }                             %% Plus as described in `telemetry:span/3`
```

A telemetry span of a full scenario execution for a user (i.e. the exported `terminate/1,2` function):

```erlang
event_name: [amoc, scenario, terminate, _]
measurements: #{}                 %% As described in `telemetry:span/3`
metadata: #{scenario := module(), %% Running scenario
            state := term()       %% The state as returned by `init/0`
           }                      %% Plus as described in `telemetry:span/3`
```

## Controller

Indicates the number of users manually added or removed

```erlang
event_name: [amoc, controller, users]
measurements: #{count := non_neg_integer()}
metadata: #{monotonic_time := integer(), scenario := module(), type := add | remove}
```

## Throttle

### Init

Raised when a throttle mechanism is initialised.

```erlang
event_name: [amoc, throttle, init]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), name := atom()}
```

### Rate

Raised when a throttle mechanism is initialised or its configured rate is changed.
This event is raised only on the master node.

```erlang
event_name: [amoc, throttle, rate]
measurements: #{rate_per_minute := float()}
metadata: #{monotonic_time := integer(), name := atom(), msg => binary()}
```

### Request

Raised when a process client requests to be allowed pass through a throttled mechanism.

```erlang
event_name: [amoc, throttle, request]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), name := atom()}
```

### Execute

Raised when a process client is allowed to execute after a throttled mechanism.

```erlang
event_name: [amoc, throttle, execute]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), name := atom()}
```

### Throttle process internals

Events related to internals of the throttle processes, these might expose unstable conditions you
might want to log or reconfigure:

```erlang
event_name: [amoc, throttle, process]
measurements: #{logger:level() => 1}
metadata: #{monotonic_time := integer(),
            log_level := logger:level(),
            msg := binary(),
            rate => non_neg_integer(),
            interval => non_neg_integer(),
            state => map(),
            _ => _}
```

## Coordinator

### Event
Indicates when a coordinating event was raised, like a process being added for coordination or a timeout being triggered

```erlang
event_name: [amoc, coordinator, start | stop | add | reset | timeout]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), name := atom()}
```

### Action triggered
Indicates an action is about to be triggered, either by enough users in the group or by timeout

```erlang
event_name: [amoc, coordinator, execute]
measurements: #{count := num_of_users()}
metadata: #{monotonic_time := integer(), event := coordinate | reset | timeout | stop}
```

## Config

### Internal events
There are related to bad configuration events, they might deserve logging

```erlang
event_name: [amoc, config, get | verify | env]
measurements: #{logger:level() => 1}
metadata: #{monotonic_time := integer(),
            log_level => logger:level(),
            setting => atom(),
            msg => binary(), _ => _}
```

## Cluster

### Internal events
There are related to clustering events

```erlang
event_name: [amoc, cluster, connect_nodes | nodedown | master_node_down]
measurements: #{count => non_neg_integer()},
metadata: #{nodes => nodes(), state => map()}
```
