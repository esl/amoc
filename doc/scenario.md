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


For developing XMPP scenarios, we recommend the
[esl/escalus](https://github.com/esl/escalus) library.
If additional dependencies are required by your scenario,
a `rebar.config` file can be created inside the `scenario` dir
and `deps` from that file will be merged with Amoc's dependencies.
