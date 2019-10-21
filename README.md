# A Murder of Crows [![Build Status](https://travis-ci.org/esl/amoc.svg?branch=master)](https://travis-ci.org/esl/amoc)

----------------------------------------------------------------------------------------------
A Murder of Crows, aka amoc, is a simple tool for  running massively parallel tests in a distributed environment. The services can be XMPP-, HTTP- or MQTT- based.
Running a local instance of Amoc, you can test your scenarios during the development process. It uses [escalus](https://github.com/esl/escalus), the Erlang XMPP client library.

[MongooseIM](https://github.com/esl/MongooseIM) is continuously being load tested with Amoc.

---------------------------------------------------------------------
In order to implement and run locally your scenario follow chapters
[1](doc/scenario.md) and [2](doc/local-run.md). Before [setting up
distributed environment](doc/distributed.md), please read through the
configuration overview. If you wish to run load tests via http api, take a
look at the [last](doc/http-api.md) chapter.

### Table of Contents
- [Developing a scenario](doc/scenario.md)
- [Running locally](doc/local-run.md)
- [Configuration](doc/configuration.md)
- [Setting up distributed environment](doc/distributed.md)
- [Running load test](doc/distributed-run.md)
- [Advanced scenario features](doc/advanced-features.md)
- [HTTP API](doc/http-api.md)
