Amoc also exposes the following telemetry events:

## Scenario

Indicates the start of a scenario after having successfully return from the `init/0` callback:
```erlang
event_name: [amoc, scenario, start]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), scenario := module()}
```

Indicates termination of a scenario after having run the optional `terminate/` callback, and contains the return value of such:
```erlang
event_name: [amoc, scenario, stop]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), scenario := module(), return := term()}
```

A telemetry span of a full scenario execution for a user (i.e. the exported `start/1,2` function):
```erlang
event_name: [amoc, scenario, user, _]
measurements: #{} %% As described in `telemetry:span/3`
metadata: #{scenario := module(),
            user_id := non_neg_integer()} %% Plus as described in `telemetry:span/3`
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

## Coordinator

Indicates when a coordinating event was raised, like a process being added for coordination or a timeout being triggered

### Event
```erlang
event_name: [amoc, coordinator, start | stop | add | reset | timeout]
measurements: #{count := 1}
metadata: #{monotonic_time := integer(), name := atom()}
```
