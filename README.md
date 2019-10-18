# A Murder of Crows [![Build Status](https://travis-ci.org/esl/amoc.svg?branch=master)](https://travis-ci.org/esl/amoc)

----------------------------------------------------------------------------------------------
A Murder of Crows, aka amoc, is a simple tool for  running massively parallel tests in a distributed environment. The services can be XMPP-, HTTP- or MQTT- based.
Running a local instance of Amoc, you can test your scenarios during the development process. It uses [escalus](https://github.com/esl/escalus), the Erlang XMPP client library.

[MongooseIM](https://github.com/esl/MongooseIM) is continuously being load tested with Amoc.

---------------------------------------------------------------------
### Table of Contents
- [Developing a scenario](#running-on-localhost)
- [Setting up locally](#local)
	- [Testing your scenario](#local-run)
- [Setting up distributed environment](#locally)
	- [Running load test](#distributed)
