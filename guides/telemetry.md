Amoc also exposes the following telemetry events:

## Scenario

A telemetry span of a full scenario execution
```erlang
event_name: [amoc, scenario, user]
measurements: #{}
metadata: #{}
```

## Controller

Indicates the number of users added or removed
```erlang
event_name: [amoc, controller, users]
measurements: #{count => non_neg_integer()}
metadata: #{type => add | remove}
```

## Throttle

### Rate

Raised when a throttle mechanism is initialised or its configured rate is changed.

```erlang
event_name: [amoc, throttle, rate]
measurements: #{rate => non_neg_integer()}
metadata: #{name => atom()}
```

### Request

Raised when a process client requests to be allowed pass through a throttled mechanism.

```erlang
event_name: [amoc, throttle, request]
measurements: #{count => 1}
metadata: #{name => atom()}
```

### Execute

Raised when a process client is allowed to execute after a throttled mechanism.

```erlang
event_name: [amoc, throttle, execute]
measurements: #{count => 1}
metadata: #{name => atom()}
```

## Coordinator

Indicates when a coordinating event was raised, like a process being added for coordination or a timeout being triggered

### Event
```erlang
event_name: [amoc, coordinator, start | stop | add | reset | timeout]
measurements: #{count => 1}
metadata: #{name => atom()}
```
