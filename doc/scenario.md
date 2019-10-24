## Developing a scenario

All files related to scenarios should be placed in the `scenarios` directory.

A scenario specification is an [Erlang](https://www.erlang.org/) module that implements
the `amoc_scenario` behaviour.
It has to export two callback functions:
- `init/0` - called only once per test run on every node, at the very beginning.
It can be used for setting up initial (global) state: metrics, database connections, etc.
It can return an `ok` or a tuple `{ok, State}` from which the `State` may be passed to every user.
- `start/1` or `start/2` - describes the actual scenario and is executed for
each user, in the context of that user's process.
The first argument is the given user's unique integer id.
The second, which is optional, is the state, as returned from the `init/0` function.
When the `start` function returns, it is executed again after some delay (60 seconds by default).

A typical scenario file will look like this:

```erlang
-module(my_scenario).

-export([init/0]).
-export([start/1]).

init() ->
    %% initialize some metrics
    ok.

start(Id) ->
    %% connect user
    %% fetch user's history
    %% send some messages
    %% wait a little bit
    %% send some messages again
    ok.
```
or, using the `start/2` function:

```erlang
-module(my_scenario).

-export([init/0]).
-export([start/2]).

init() ->
    %% initialize some metrics
    Settings = get_settings(),
    {ok, Settings}.

start(Id, Settings) ->
    %% connect user using Settings
    %% fetch user's history
    %% send some messages
    %% wait a little bit
    %% send some messages again
    ok.
```

### Advanced features
In addition to the required ones, the `amoc_scenario` behaviour proposes optional callbacks:

- `continue/0` and `terminate/0`

    These callbacks make it possible to terminate a scenario at any moment.
    The former checks if the current scenario is still valid, e.g. some metrics are
    below a certain threshold.
    It must return `continue` or `{stop, Reason}`.
    The latter callback implements the actual termination.
    It's called only if `continue/0` returned `{stop, Reason}`.
    The reason is passed to the `terminate` callback.
    When `continue` is returned nothing happens.
    Only if both of them are implemented, automatic checking is enabled.
    By default, it is scheduled to run every 60s.
    This can be changed by setting the `scenario_checking_interval` application environment variable.
    The checking logic happens in the `amoc_controller` process only on a
    "master" Amoc node and only in the "distributed mode" (the scenario has
    to be started via `amoc_dist:do/3/4`).

-   `next_user_batch/2`
    
    This callback enables adding users in batches.
    It is supposed to return a strategy for adding users,
    which is a list of `{Node, NumOfUsers, Interarrival}`.
    It is then used by the `amoc_controller:add_batches/2` function,
    which takes the scenario module and the number of batches as parameters.
    The parameters passed to the `next_user_batch` callback are 
    the batch index and the number of users added in the previous batch.
    Batches are added every batch interval as specified by the `add_batch_interval`
    application environment variable, which is 5 minutes by default.
    Adding batches can be scheduled via HTTP API by specifying the batch keys
    in a body request to the `scenarios/$SCENARIO` endpoint.
    See [REST API docs](./REST_API_DOCS.md#start-scenario).


For developing XMPP scenarios, we recommend the
[esl/escalus](https://github.com/esl/escalus) library.
If additional dependencies are required by your scenario,
a `rebar.config` file can be created inside the `scenario` dir
and `deps` from that file will be merged with Amoc's dependencies.
