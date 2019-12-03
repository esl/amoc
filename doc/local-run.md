### Test your scenario locally

Everything you need to do is to create the release. To achieve that run:
`make rel`. Now you are ready to test our scenario locally with one Amoc node;
to start the node run `_build/default/rel/amoc/bin/amoc console`.

Start `my_scenario` spawning 10 amoc users with IDs from range (1,10) inclusive.
```erlang
amoc:do(my_scenario, 10, []).
```

Add 10 more user sessions.
```erlang
amoc:add(10).
```

Remove 10 users.
```erlang
amoc:remove(10, true).
```

NOTE: the exact range of the users can be added using ``amoc_controller:add_users/2`` interface 