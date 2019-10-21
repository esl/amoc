### Test your scenario locally

Everything you need to do is to create the release. To achieve that run:
`make rel`. Now you are ready to test our scenario locally with one Amoc node;
to start the node run `_build/default/rel/amoc/bin/amoc console`.

Start `my_scenario` spawning 10 amoc users with IDs from range (1,10) inclusive.
```erlang
amoc_local:do(my_scenario, 1, 10).
```

Add 10 more user sessions.
```erlang
amoc_local:add(10).
```

Remove 10 users.
```erlang
amoc_local:remove(10, [{force,true}]).
```

NOTE: We advise using this mode for the scenario debugging purposes - everything you
log using lager is going to be visible in the erlang shell.
In this mode the annotations are not working, however this may change in the future.
