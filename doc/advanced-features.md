### Advanced scenario features

#### 1. Terminate a scenario at any time

It's possible to terminate a scenario at any moment via two
optional scenario callbacks `continue/0` and `terminate/1`.
The former checks if the current scenario is still valid, e.g. some metrics are
below a certain treshold and must return `continue` or `{stop, Reason}`.
The latter one implements the actual termination.
It's called only if `continue/0` returned `{stop, Reason}`.
The reason is passed to the `terminate` callback.
When `continue` is returned nothing happens. Both callbacks are optional.
This checking is scheduled only if both of them are implemented.
By default, the checking interval equals 60s.
It can be changed by setting the `scenario_checking_interval`
application environment variable.
The checking logic happens in the `amoc_controller` process only on a
"master" Amoc node and only in the "distributed mode" (the scenario has
to be started via `amoc_dist:do/3/4`).

#### 2. Add users in batches

There is an `amoc_controller:add_batches/2` function that allows to add
users in batches according to some strategy.
The function takes the scenario module and the number of batches as parameters.
The strategy to add users should be returned by the `next_user_batch/2`
callback implemented in the scenario module.
The callback is optional.
The batch index and the number of users added in the previous batch are
parmeters that are passed to the callback function.
The strategy is a list of `{Node, NumOfUsers, Interarrival}`.
Batches are added every batch interval specified by the `add_batch_interval`
application environment variable, which is 5 minutes by default.
Adding batches can be scheduled via HTTP API by specifying batches key
in a body request to the `scenarios/$SCENARIO` endpoint.
See [REST API docs](./REST_API_DOCS.md#start-scenario).
