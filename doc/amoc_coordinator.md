# amoc_coordinator

## Module

`amoc_coordinator` 

## Description

This module allows to synchronize the users and act on groups of them.

The coordinator reacts to new users showing up in a system, according to the `Coordination Plan`. 
The `Coordination Plan` consists of `Coordination Items`, and each of them is defined as one of the following: `{NumberOfUsers, CoordinationActions}`.
 - When the `Number Of Users` is set to `all`, then only `Coordination Actions` with the arities `/1, /2` are handled. 
 The `Coordination Items` with `all` are triggered by the `timeout` event. 
 - When the `Number Of Users` is set to a positive integer, all `Coordination Actions` with arities `/1, /2` and `/3` are handled.

The timeout timer is reset by calling the `add` function.
A new batch size is set in the `NumberOfUsers`. Each user in the batch calls the `add` function registering to the coordinator and triggering the `Coordination Plan`.
If more then one of the `Coordination Items` matching the `NumberOfUsers` is triggered, each of them will be passed a respective number of users.
For example if the `Coordination Plan` is `[{2, Act1}, {3, Act2}]` then on the 6th user calling `add`, `Act1` will be called with 2 users passed and `Act2` will be called with 3 users passed.

`Coordination Actions` may be one of the following:
 - `fun(Event) -> any()` - this type of action does not care about particular users, but only about the number of them;
 - `fun(Event, ListOfUsersData) -> any()` - this type of action gets `ListOfUsersData` which is a list of `{Pid, Data}` tuples with `Pid`s passed by users calling `amoc_coordinator:add/2` or `amoc_coordinator:add/3`;
- `fun(Event, User1, User2) -> any()` - this type of action gets `distinct pairs` from the batch of users `User1` and `User2` which are `{Pid, Data}` tuples with `Pid`s passed by users calling `amoc_coordinator:add/2` or `amoc_coordinator:add/3`;

The order of `Coordination Actions` execution is not guaranteed.
It’s guaranteed that all the `Coordination Actions` with `all` are executed after all numbered `Coordination Actions` are done.

`distinct pairs` - in the context,  these are pairs from a given collection of users, where:
 - when `{A, B}` is in the `distinct pairs` then `{B, A}` is not;
 - `{A, A}` is not in the `distinct pairs`;
 - all pairs are distinct;
 - Eg. for `[a]`, the `distinct pairs` collection is `[{a, undefined}]`;
 - Eg. for `[a, b]`, the `distinct pairs` collection is `[{a, b}]`;
 - Eg. for `[a, b, c]`, the `distinct pairs` collection is `[{a, b}, {a, c}, {b, c}]`.

## Exports

#### `start(CoordinatorName, CoordinationPlan) -> ok | error`
#### `start(CoordinatorName, CoordinationPlan, Timeout) -> ok | error`

##### Types
```erlang
CoordinatorName :: atom()
CoordinationPlan  :: [ CoordinationItem ]
CoordinationItem :: {NumberOfUsers :: pos_integer() | all, CoordinationActions}
CoordinationActions :: [ CoordinationActions ] | CoordinationAction
CoordinationAction :: 
    fun(Event) -> any() |
    fun(Event, ListOfUsersData) -> any() |
    fun(Event, MaybeUser, MaybeUser) -> any()
Event :: coordinate | timeout | stop | reset
ListOfUsersData :: [ User ]
MaybeUser :: undefined | User
User :: {UsersPid :: pid(), Data :: any()}
Timeout :: pos_integer() | infinity
```

This function starts a coordinator. Usually is called in `init/0` of a amoc scenario.

#### `add(CoordinatorName, Data) -> ok`
#### `add(CoordinatorName, Pid, Data) -> ok`

##### Types
```erlang
CoordinatorName :: atom()
UsersPid :: pid()
Data :: any()
```

This function adds the current process data. It is usually called in `start/2` of a amoc scenario.

## Example

This scenario will demonstrate how do the `users` interact with `amoc_coordinator`:

```erlang
-module(example).

-export([init/0]).
-export([start/2]).

init() ->
    Plan = [
        {2, fun(Event) ->
                io:fwrite("Two new users showed up: Event = ~p\n", [Event])
            end},
        {2, fun(Event, ListOfUsers) ->
                io:fwrite("Two new users showed up: Event = ~p; ListOfUsers = ~p\n", [Event, ListOfUsers]),
                [ Pid ! {hello, Data} || {Pid, Data} <- ListOfUsers]
            end},
        {2, fun(Event, User1, User2) ->
                io:fwrite("Two new users showed up: Event = ~p; User1 = ~p; User2 = ~p\n", [Event, User1, User2])
            end},
        
        {3, fun(_Event) ->
                io:fwrite("Three new users showed up\n", [])
            end},
        {all, fun(Event) ->
                io:fwrite("All users have called amoc_coordinator:add in Event = ~p\n", [Event])
            end}
        
    ],
    Settings = [setting1, {setting2, something}],
    amoc_coordinator:start(?MODULE, Plan),
    {ok, Settings}.

start(Id, _Settings) ->
    io:fwrite("User = ~p\n", [Id]),
    amoc_coordinator:add(?MODULE, Id),
    receive
        Msg -> 
            io:fwrite("{Msg = ~p, Id = ~p\n", [Msg, Id])
    end,
    ok.
```


To run it:

```bash
$ make rel
$ _build/default/rel/amoc/bin/amoc console

1> amoc_local:do(example, 1, 5).
```

Filtered, formated and explained output:

```erlang
User = 1  % First user is started

ok  % result of calling amoc_local:do/3

User = 2 % First user is started

% We have 2 users added to amoc_coordinator so all of actions {2, _} are triggered:
Two new users showed up: Event = coordinate; User1 = {<0.1142.0>,2}; User2 = {<0.1140.0>,1}
% This action triggers sending {hello,Id} to the users 1 and 2
Two new users showed up: Event = coordinate; ListOfUsers = [{<0.1142.0>,2},{<0.1140.0>,1}]
Two new users showed up: Event = coordinate

% Users 1 and 2 received messages and print them:
{Msg = {hello,2}, Id = 2
{Msg = {hello,1}, Id = 1

User = 3
% We have 3 users added to amoc_coordinator so all of the {3, _} actions are triggered:
Three new users showed up

User = 4
% We have 4 and 4 rem 2 == 0 therefore users added to amoc_coordinator so all of the {3, _} actions are triggered:
Two new users showed up: Event = coordinate; User1 = {<0.1144.0>,4}; User2 = {<0.1143.0>,3}
Two new users showed up: Event = coordinate; ListOfUsers = [{<0.1144.0>,4},{<0.1143.0>,3}]
Two new users showed up: Event = coordinate

{Msg = {hello,4}, Id = 4
{Msg = {hello,3}, Id = 3
User = 5

% You need to wait for a while, and ...
% Timeout has been reached, which triggers all of the Coordination Actions with the remaining number of users.
Three new users showed up
Two new users showed up: Event = timeout; User1 = {<0.1139.0>,5}; User2 = undefined
Two new users showed up: Event = timeout; ListOfUsers = [{<0.1139.0>,5}]
{Msg = {hello,5}, Id = 5
Two new users showed up: Event = timeout
All users have called amoc_coordinator:add in Event = timeout
```

