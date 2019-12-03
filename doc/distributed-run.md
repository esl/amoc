## Running load test

Starting a scenario on multiple nodes is slightly more difficult.
You need a number of machines that will be actually running the test
(*slaves*) and one controller machine (*master*, which might be one of the test nodes).
Another aproach to do it is to use docker containers.

Now instead of `amoc` use `amoc_dist` - this will tell amoc to distribute
and start scenarios on all known nodes (except master).

```erlang
amoc_dist:do(my_scenario, 100, Settings).
```

Start `my_scenario` spawning 100 amoc users with IDs from the range [1,100] inclusive.
In this case sessions are going to be distributed across all nodes except master.

`Settings` is an optional proplist with scenario options that can be extracted using amoc_config module.
The values provided in this list shadow OS and APP environment variables. Note that these settings will be propagated
 automatically among all the nodes in the amoc cluster.


```erlang
amoc_dist:add(50).
```
Add 50 more users to the currently started scenario.

```erlang
amoc_dist:remove(50, Force).
```

Remove 50 sessions. 

Where ``Force`` is a boolean of value:

* ``true``  - to kill the user processes using ``supervisor:terminate_child/2`` function
* ``false`` - to send ``exit(User,shutdown)`` signal to the user process (can be ignored by the user)

All the users are ``temporary`` children of the ``simple_one_for_one`` supervisor with the ``shutdown`` 
key set to ``2000``.

Also all the user processes trap exit signal.


### Don't stop scenario on exit

There is one problem with the `bin/amoc console` command. When you exit the Erlang
shell the scenario is stopped (in fact the erlang nodes are killed).
To prevent that start the amoc node in the background using `bin/amoc start`.
Now you can run commands by attaching to the amoc's node with `bin/amoc attach`,
typing a command and then pressing Ctrl+D to exit the shell.
After that the scenario will keep running.
