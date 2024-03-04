## API

See `amoc_throttle`

## Overview

Amoc throttle is a module that allows limiting the number of users' actions per given interval, no matter how many users there are in a test.
It works in both local and distributed environments, allows for dynamic rate changes during a test and exposes metrics which show the number of requests and executions.

Amoc throttle allows setting the execution `Rate` per `Interval` or limiting the number of parallel executions when `Interval` is set to `0`.
Each `Rate` is identified with a `Name`.
The rate limiting mechanism allows responding to a request only when it does not exceed the given `Rate`.
Amoc throttle makes sure that the given `Rate` per `Interval` is maintained on a constant level.
It prevents bursts of executions which could blurry the results, as they technically produce a desired rate in a given interval.
Because of that, it may happen that the actual `Rate` would be slightly below the demanded rate. However, it will never be exceeded.

## Examples

A typical use of Amoc throttle will look something like this:

```erlang
-module(scenario_with_throttle).

-behaviour(amoc_scenario).

-export([init/0]).
-export([start/1]).

init() ->
    amoc_throttle:start(messages_rate, 100), %% 100 messages per minute
    %% continue initialization
    ok.

start(Id) ->
    %% initialize user
    user_loop(Id),
    ok.

user_loop(Id) ->
    amoc_throttle:send_and_wait(messages_rate, some_message),
    send_message(Id),
    user_loop(Id).
```
Here a system should be under a continuous load of 100 messages per minute.
Note that if we used something like `amoc_throttle:run(messages_rate, fun() -> send_message(Id) end)` instead of `amoc_throttle:send_and_wait/2` the system would be flooded with requests.

A test may of course be much more complicated.
For example it can have the load changing in time.
A plan for that can be set for the whole test in `init/1`:
```erlang
init() ->
    %% init metrics
    amoc_throttle:start(messages_rate, 100),
    %% 9 steps of 100 increases in Rate, each lasting one minute
    amoc_throttle:change_rate_gradually(messages_rate, 100, 1000, 60000, 60000, 9),
    ok.
```

Normal Erlang messages can be used to schedule tasks for users by themselves or by some controller process.
Below is a sketch of a user's behaviour during a test.
It waits for messages in a loop, and sends one after receiving the message `{send_message, To}`.
The rate of messages sent during a test will not exceed the one set in the `message_rate`.
Sending messages is scheduled in the `set_up/1` function and in the user loop if some arbitrary condition is met.
This models the behaviour common across load tests, when users respond only to some messages.

```erlang
set_up(Users) ->
    [User ! {send_message, To} || User <- Users, To <- Users].

user_loop() ->
    receive
        {send_message, To} ->
            send_xmpp_message(To),
            user_loop();
        Message ->
            process_message(Message),
            user_loop()
    end.

process_message(Message) ->
    case some_condition(Message) of
        true ->
            To = get_sender(Message),
            amoc_throttle:send(message_rate, {send_message, To});
        false ->
            ok
    end.
```

For a more comprehensive example please refer to the `throttle_test` scenario, which shows possible usages of the Amoc throttle.

## How it works

### Module overview

- `amoc_throttle.erl` - provides an API for `amoc_throttle_controller`.
- `amoc_throttle_controller.erl` - a gen_server which is responsible for reacting to requests, and managing `throttle_processes`.
In a distributed environment an instance of `throttle_controller` runs on every node, and the one running on the master Amoc node stores the state for all nodes.
- `amoc_throttle_process.erl` - gen_server module, implements the logic responsible for limiting the rate.
For every `Name`, a `NoOfProcesses` are created, each responsible for keeping executions at a level proportional to their part of `Rate`.

### Distributed environment

#### Metrics
In a distributed environment every Amoc node with a throttle started, exposes metrics showing the numbers of requests and executions.
Those exposed by the master node show the sum of all metrics from all nodes.
This allows to quickly see the real rates across the whole system.

#### Workflow
When a user executes `amoc_throttle:run/2`, a request is reported to a metric that runs on the user's node.
Then a runner process is spawned on the same node.
Its task will be to execute `Fun` asynchronously.
A random throttle process which is assigned to the `Name` is asked for a permission for asynchronous runner to execute `Fun`.
When the request reaches the master node, where throttle processes reside, the request metric on the master node is updated and the throttle process which got the request starts monitoring the asynchronous runner process.
Then, depending on the system's load and the current rate of executions, the asynchronous runner is allowed to run the `Fun` or compelled to wait, because executing the function would exceed the calculated `Rate` in an `Interval`.
When the rate finally allows it, the asynchronous runner gets the permission to run the function from the throttle process.
Both processes increase the metrics which count executions, but for each the metric is assigned to their own node.
Then the asynchronous runner tries to execute `Fun`.
It may succeed or fail, either way it dies and an `'EXIT'` signal is sent to the throttle process.
This way it knows that the execution of a task has ended, and can allow a different process to run its task connected to the same `Name` if the current `Rate` allows it.

Below is a graph showing the communication between processes on different nodes described above.
![amoc_throttle_dist](assets/amoc_throttle_dist.svg)
