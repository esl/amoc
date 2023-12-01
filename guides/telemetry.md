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
measurements: #{rate := non_neg_integer()}
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
measurements: #{msg := binary(), process := pid()}
metadata: #{monotonic_time := integer(), name := atom(), printable_state => map()}
```

## Coordinator

Indicates when a coordinating event was raised, like a process being added for coordination or a timeout being triggered

### Event
```erlang
event_name: [amoc, coordinator, start | stop | add | reset | timeout]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), name := atom()}
```

## Config

### Internal events
There are related to bad configuration events, they might deserve logging
```erlang
event_name: [amoc, config, get | verify | env]
measurements: #{}
metadata: #{log_class => syslog_level(), _ => _}
```

## Cluster

### Internal events
There are related to clustering events
```erlang
event_name: [amoc, cluster, connect_nodes | nodedown | master_node_down]
measurements: #{count => non_neg_integer()},
metadata: #{node => node(), nodes => nodes(), state => map()}
```
