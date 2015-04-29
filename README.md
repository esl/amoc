# A Murder of Crows

A simple tool for running parallel XMPP tests

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

To start your scenario, execute the following:

```bash
./rebar get-deps  # This is needed only for the first run
./run.sh my_scenario 1 1000
```

These commands will fetch all the required dependencies, compile all the
source files and scenarios.

The scenario ``my_scenario`` will be started in an Erlang interactive shell,
running with user ids from 1 to 1000.

### In a distributed environment

Starting a scenario on multiple nodes is slightly more difficult.
You need a number of machines that will be actually running the test
(*slaves*) and one controller machine (*master*, which might be one of the test nodes).

Moreover, you need a machine with both **Erlang** and **ansible** installed in
order to do the following:

1. Edit the ``hosts`` file and provide the hostnames or the IP addresses of
all the nodes, including them in the Erlang hostnames as well.
2. Edit the ``ansible/group_vars/all`` file and provide the SSH username
   for all the nodes and the Graphite's host.
3. Edit the ``ansible/group_vars/amoc-master`` file and enter hosts of all
   the slave nodes as in the example. This should be the same as the first
column in the ``hosts`` file.
4. Run ``make prepare`` in order to configure the slave nodes.
5. Run ``make rel`` in order to build the release.
6. Run ``make deploy`` in order to deploy the release.
7. Go to the master's node and start amoc by executing
   ``~/amoc_master/bin/amoc start``. Now you can run commands by attaching
to the amoc's node with ``~/amoc_master/bin/amoc attach``, typing a
command and pressing Ctrl+D.
8. Execute ``amoc_dist:do(my_scenario, 1, 100).`` to start
   a scenario.
9. Execute ``amoc_dist:add(50).`` to add some more users.
10. Execute ``amoc_dist:remove(50, [{force, true}]).`` to remove some of
    them.
11. Execute ``~/amoc_master/bin/amoc stop`` when you're done.  
