## Configuration

Amoc is configured through OTP application environment variables that
are loaded from the configuration file, operating system environment variables
(with prefix ``AMOC_``) and Erlang application environment variables
(`priv/app.config`).

`AMOC_HOSTS` (the hostnames of all of the `amoc` machines) must be set in order to successfully
run the scenario in distributed environment.

Amoc is able to report metrics to Graphite. It is possible to set up
reporting endpoint by setting `AMOC_GRAPHITE_HOST` and `AMOC_GRAPHITE_PORT` enviornmental variables.

If there is a need to point amoc to some additional paths with code,
it can be done by specifying variable `AMOC_EXTRA_CODE_PATHS`.

Internally, amoc is using the following settings:

- ``interarrival`` - a delay in ms, for each node, between creating process
  for two consecutive users. Defaults to 50 ms.
- ``repeat_interval`` - a delay in ms each user process waits
  before starting the same scenario again. Defaults to 1 minute.
- ``repeat_num`` - number of scenario repetitions for each process.
  Defaults to ``infinity``.

You can also define your own entries that you might later use in your
scenarios.

The ``amoc_config`` is a module for getting configuration. Every time we ask
for a config value, ``amoc_config`` looks into OS environment variables first
(with ``AMOC_`` prefix; e.g. if we want to set ``interarrival`` by this mechanism
we should set OS environment variable ``AMOC_interarrival``), and if it doesn't
find it there, it tries to get it from the Erlang application environment variables.
If it cannot find it there either and the default value was not supplied, an error
it thrown. What's more, we can set variables  dynamically (by setting OS variable or
``application:set_env(amoc, VARIABLE_NAME, VARIABLE_VALUE)`` in Erlang).

``amoc_config`` provides the following function:

- ``get(Name)`` and ``get(Name, Default)`` - return the value for the
  given config entry according to the aforementioned rules.
