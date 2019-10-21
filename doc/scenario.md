## Developing a scenario

All files related to scenarios should be placed in the `scenarios` directory.

A scenario specification is an [Erlang](http://www.erlang.org/) module that
exposes two callback functions:
- ``init/0`` - called only once per test run, at the very beginning.
It can be used for setting up initial (global) state: metrics, database
connections, etc.
- ``start/1`` - describes the actual scenario and is executed for
each user, in the context of that user's process.
It takes one argument, which is the given user's unique integer id.
When the function returns, it is executed again after some delay (60 seconds by default).

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

For developing XMPP scenarios, we recommend the
[esl/escalus](https://github.com/esl/escalus) library.
If additional dependencies are required by your scenario,
a `rebar.config` file can be created inside the `scenario` dir
and `deps` from that file will be merged with Amoc's dependencies.
