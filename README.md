# A Murder of Crows

A simple tool for running parallel XMPP tests

## Developing a scenario

All files related to test scenarios are placed in `scenario` directory.

Test scenarios is described entirely by a module in Erlang programming
language.

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

The ``init/0`` function will be called only once at the very beginning. It should
be used for initialization.

The  ``start/1`` describes the actual scenario and will be executed for
each user.
``Id`` is user's unique, integer id.
When function returns it will be executed again after some delay (60
seconds by default).

For developing XMPP scenarios we recommend the
[esl/escalus](https://github.com/esl/escalus) library.

If additional dependencies are required by scenario, a `rebar.config` file can be created inside the `scenario` dir and deps from that file will be merged with main deps.

## Running a scenario

To start your scenario type in shell:

```bash
./rebar get-deps  # This is needed only for the first time
./run.sh my_scenario 1 1000
```

These commands will fetch all required dependencies, compile all the
source file and scenarios.

The ``my_scenario`` with user ids from 1 to 1000 will also be started in
an Erlang interactive shell.
