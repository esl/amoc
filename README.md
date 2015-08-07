# A Murder of Crows

A Murder of Crows, aka amoc, is simple tool for running massively parallel XMPP tests. It can be used to load test [ESL's MongooseIM](https://github.com/esl/MongooseIM).

It uses [escalus](https://github.com/esl/escalus), the Erlang XMPP client library.


## Developing a scenario

All files related to scenarios should be placed in the `scenarios` directory.

A scenario specification is just an [Erlang](http://www.erlang.org/) module that
exposes the necessary callback functions.

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

The ``init/0`` function will be called only once per test run, at the very beginning.
It can be used for setting up necessary (global) state: metrics, database
connections, etc.

The ``start/1`` function describes the actual scenario and will be executed for
each user, in the context of that user's process.

``Id`` is the given user's unique integer id.
After this function returns, it will be executed again following some delay (60
seconds by default).

For developing XMPP scenarios, we recommend the
[esl/escalus](https://github.com/esl/escalus) library.

If additional dependencies are required by your scenario, a `rebar.config` file
can be created inside the `scenario` dir and `deps` from that file will be
merged with amoc's `deps`.

## Running a scenario

### Locally

Everything you need to do is to create the release. To achieve that run:
`make rel`. Now you are ready to test our scenario locally with one amoc node, to
do this run `_rel/amoc/bin/amoc console`.

```erlang
amoc_local:do(my_scenario, 1, 10).
```

Start `my_scenario` spawning 10 amoc users with IDs from range (1,10) inclusive.

```erlang
amoc_local:add(10).
```

Add 10 more user sessions.

```erlang
amoc_local:remove(10, [{force,true}]).
```

Remove 10 users.

NOTE: We advise using this mode for the scenario debugging purposes - everything you
log using lager is going to be visible in the erlang shell. In this mode the annotations
are not working, however this may change in the future.

### In a distributed environment

Starting a scenario on multiple nodes is slightly more difficult.
You need a number of machines that will be actually running the test
(*slaves*) and one controller machine (*master*, which might be one of the test nodes).

Moreover, you need a machine with both **Erlang** and **ansible** installed in
order to do the following:

1. Edit the ``hosts`` file and provide all required information - more information in the
file itself.
2. Run ``make prepare`` to configure the slave nodes, you don't need to do that each
time you want to run a load test.
3. Run `make deploy` in order to build and deploy the release across all nodes.
Then go to the master's node and start amoc by executing ``~/amoc_master/bin/amoc console``, that
will run the amoc shell, wait a couple of seconds till all slave nodes are started.

Now instead of `amoc_local` use `amoc_dist` - this will tell amoc to distribute
and start scenarios on all known nodes (except master).

```erlang
amoc_dist:do(my_scenario, 1, 100, Opts).
```

Start `my_scenario` spawning 100 amoc users with IDs from range (1,100) inclusive.
In this case sessions are going to be distributed across all nodes except master.
At this moment users are distributed in round-robin fashion - user1 goes to slave1,
user2 goes to slave2, ...., user10 goes to slave10, user11 goes to slave1 etc
(with the assumption that there are 10 slave nodes configured).
We are planning to make the strategy configurable.

Opts may contain:

* {nodes, Nodes} - list of nodes on which scenario should be started, by default all available slaves
* {comment, String} - comment displayed used in the graphite annotations


```erlang
amoc_dist:add(50).
```
Add 50 more users to the currently started scenario.

```erlang
amoc_dist:remove(50, Opts).
```

Remove 50 sessions.

Where Opts may contain:

* {force, true}  - immediately kill the processes responsible for user connections
* {force, false} (default) - stop user processes gently - do not start them again


### Don't stop scenario on exit

There is one problem with the `bin/amoc console` command. When you exit the Erlang
shell the scenario is stopped (in fact the erlang nodes are killed).
To prevent that start the amoc node in the background using `bin/amoc start`.
Now you can run commands by attaching to the amoc's node with `bin/amoc attach`,
typing a command and then pressing Ctrl+D to exit the shell.
After that the scenario will keep running.
