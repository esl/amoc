### Test your scenario locally

Everything you need to do is to create the release. To achieve that run:
`make rel`. Now you are ready to test our scenario locally with one Amoc node;
to start the node run `_build/default/rel/amoc/bin/amoc console`.

Start `my_scenario` spawning 10 amoc users with IDs from range (1,10) inclusive.
```erlang
amoc:do(my_scenario, 10, []).
```
```elixir
:amoc.do(:my_scenario, 10, []).
```

Add 10 more user sessions.
```erlang
amoc:add(10).
```
```elixir
:amoc.add(10).
```

Remove 10 users.
```erlang
amoc:remove(10, true).
```
```elixir
:amoc.remove(10, true).
```

#### Many independent Amoc nodes

Sometimes a need arises to run several Amoc nodes independently from each other.
In this case we would like to be able to run different ranges of user ids on every node.
To do so, the following trick could be applied:

1. `amoc:start(my_scenario,0,[]).`
2. `amoc_controller:add_users(StartId, StopId).`

NODE: in case of independent Amoc nodes, it's also possible to run different scenarios on different nodes.
