## Running load test

Starting a scenario on multiple nodes is slightly more difficult.
You need a number of machines that will be actually running the test
(*slaves*) and one controller machine (*master*, which might be one of the test nodes).
Another aproach to do it is to use docker containers.

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
