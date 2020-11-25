# A Murder of Crows [![](https://github.com/esl/amoc/workflows/CI/badge.svg)](https://github.com/esl/amoc/actions?query=workflow%3ACI)

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
