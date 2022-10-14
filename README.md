# OTP18 compatible version
here is the list of significant changes:
* amoc REST API is not compatible with OTP18, so it's removed completely (all the modules and test suites).
* configuration of the modules/scenarios with `required_variable` attribute is not supported, since OTP18 doesn't support maps in module attribute (compilation crashes with `bad attribute` error). corresponding amoc_config* modules and test suites are removed.
* latest `amoc_throttle` migration to `pg` (from `pg2`) is reverted (to version `2.1.0`), also `handle_contnue/2` replaced with explicit `maybe_run_fn/1` calls (see `git diff 2.1.0 OTP18 -- src/amoc_throttle/amoc_throttle_process.erl`).
* added compatible version of rebar3.
* erlang's logger is replaced with lager.
* added `handle_info`, `terminate` and `code_change` functions as they are mandatory for OTP18.
* `exometer` dependencies are downgraded.
* `persistent_term`s replaced with `ETS` in `amoc_dist`.
* `integration_test` is reworked according to the mentioned above changes.
* lint is disabled as it doesn't work with OTP18.
* dialyzer reported 2 errors:
  ```
  src/amoc_controller.erl
   200: The pattern [{_Id, Pid} | _] can never match the type [[any()]]
   289: The pattern [{_Id, Pid} | _] can never match the type [[any()]]
  ```
  but that is a known and fixed OTP18 issue. So `-dialyzer(...)` attribute is added in [amoc_controller](src/amoc_controller.erl#L8) to ignore it. for more details see [PR-1113](https://github.com/erlang/otp/pull/1113/files) and [stdlib release notes](https://www.erlang.org/doc/apps/stdlib/notes.html#stdlib-3.1):
   * Correct the contracts for ets:match_object/1,3.



# A Murder of Crows
[![](https://github.com/esl/amoc/workflows/CI/badge.svg)](https://github.com/esl/amoc/actions?query=workflow%3ACI)

----------------------------------------------------------------------------------------------
A Murder of Crows, aka amoc, is a simple framework for running massively parallel tests in a distributed environment.

It can be used as a rebar3 dependency:
```erlang
{deps, [
    {amoc, {git, "https://github.com/esl/amoc", {tag, "2.0.1"}}}
]}.
```
[MongooseIM](https://github.com/esl/MongooseIM) is continuously being load tested with Amoc.
All the XMPP scenarios can be found [here](https://github.com/esl/amoc-arsenal-xmpp).

---------------------------------------------------------------------
In order to implement and run locally your scenarios, follow the chapters about
[developing](doc/scenario.md) and [running](doc/local-run.md) a scenario
locally.
Before [setting up the distributed environment](doc/distributed.md),
please read through the configuration overview.
If you wish to run load tests via http api,
take a look at the [REST API](doc/http-api.md) chapter.

### Table of Contents
- [Developing a scenario](doc/scenario.md)
- [Running locally](doc/local-run.md)
- [Configuration](doc/configuration.md)
- [Setting up distributed environment](doc/distributed.md)
- [Running load test](doc/distributed-run.md)
- [REST API](doc/http-api.md)
- [Amoc throttle](doc/amoc_throttle.md)
- [Amoc coordinator](doc/amoc_coordinator.md)
