# Changelog

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/)

## [2.0.1](https://github.com/esl/amoc/compare/2.0.0...2.0.1) - 2019-12-05

### Changed:
- add some fixes in the documentation
- set the release version automatically

## [2.0.0](https://github.com/esl/amoc/compare/2.0.0-beta...2.0.0) - 2019-12-03

### Changed:
- extended amoc documentation
- automatic distribution of the uploaded scenarios to all the nodes in the amoc cluster
- amoc configuration:
    - mandatory declaration of the required parameters for the scenario
    - ets based ``amoc_config:get/2`` interface
- ``amoc_controller`` - module doesn't hold any information about the cluster any more,  it's now a responsibility of the ``amoc_dist`` module


### Added:
- integration tests for docker based amoc cluster
- possibility to dynamically add the new amoc node to the cluster 

### Removed:
- unused rebar dependencies

## [2.0.0-beta](https://github.com/esl/amoc/compare/1.3.0...2.0.0-beta) - 2019-10-29

### Changed:
- exometer - now Amoc uses only exometer_core and 2 reporters graphite and statsd
- scenario for GDPR removal

### Removed:
- scenarios and helpers related to XMPP - they were moved to https://github.com/esl/amoc-arsenal-xmpp
- dependency on escalus, amqp_client and other libraries not directly used by Amoc

## [1.3.0](https://github.com/esl/amoc/compare/1.2.1...1.3.0) - 2019-10-28

### Changed:
- escalus to esl/escalus@58c2bf8
- `amoc_xmpp` new helper function `send_request_and_get_response`
- `amoc_xmpp_handlers` new function for constructing handlers
- `amoc_scenario` behavior was extended with optional callbacks `continue`, `terminate`, `next_user_batch` more details in #90
- Amoc's docker container allows to pass `AMOC_EXTRA_CODE_PATHS` env var with path to additional beam files 
- Amoc's REST API allows to start a scenario which is outside of Amoc's `scenario` directory
- `amoc_config`
    - allows to pass env vars containing value `false` 
    - allows to parse and validate scenario variables passed as env vars
- `amoc_dist` and `amoc_slave` now master node is the one a scenario is started on
- `amoc_throttle` works in distributed mode now
- documentation - the structure of the documentation was reworked and the content was updated and extended


### Added:
- `amoc_xmpp_muc` new module with helper function for building MUC scenarios
- scripts and documentation showing how to setup and run load tests with multiple Amoc nodes in docker containers
- `iq_metrics` helper for generic metrics related to IQ stanzas
- `MUC` and `MUC_light` load test scenarios
- `amoc_coordinator` to coordinate sessions


### Removed:
- ansible scripts for deploying amoc
- `amoc_annotations` module
- `config` helper module - functionality moved to `amoc_config`

## [1.2.1](https://github.com/esl/amoc/compare/1.2.0...1.2.1) - 2019-10-18

### Changed:

- `amoc_controller` to allow passing stated from scenario's `init` callback to `start` callback
- `amoc_metrics` support gauge metric type
- `amoc_scenario` behavior to allow passing state from `init` to `start` callbacks
- `amoc_xmpp`
    - `connect_or_exit` function was extended to allow passing extra user/connection options
    - new function `pick_server/1`which picks random server from config var `xmpp_servers`
- supported Erlang/OTP versions - now the oldest supported is Erlang/OTP 21.2
- escalus updated to esl/escalus@f20bee4
    - scenarios where adjusted to be compatible with the updated escalus

### Added:

- helper module for scenario configuration
- `amoc_throtlle` module
- `amoc_xmpp_user` module for unified user and password generation

## [1.2.0](https://github.com/esl/amoc/compare/1.1.0...1.2.0) - 2019-01-16

### Changed:
- `amoc_metrics` - now only 2 type of metrics are available via the `amoc_metrics` API
  - `counters` - counting occurrences of an event in total and in last minute. This is exometer's spiral metric. Its name is prefixed with `[amoc, counters]`.
  - `times` - provides statistic of given action execution time. This is exometer's histogram metric. Its name is prefixed with `[amoc, times]`.

### Added:
- `amoc_xmpp` - a new module, currently with only one function simplifying connection to the XMPP server
- `amoc_xmpp_handlers` - a new module with 2 handlers which can be used with escalus's stnaza handler feature. See sample scenarios (mongoose_simple_with_metrics) for examples. 

## [1.1.0](https://github.com/esl/amoc/compare/1.0.0...1.1.0) - 2019-01-04

### Changed:
- updated deps #67:
  - escalus
  - lager to `3.6.8`
  - jiffy to `0.15.2`
  - trails to `2.1.0`
  - cowboy_trails to `2.1.0`
  - recon to `2.4.0`
  - cowboy to `2.3.0`

### Added:
- amqp_client `3.7.9`

### Removed:
- mochijson2
- lhttpc

## [1.0.0](https://github.com/esl/amoc/compare/0.9.1...1.0.0) - 2019-01-04

### Changed:
- use rebar3 to build the project #62
- `Dockerfile` to build local version of Amoc - enables automated builds #66

## [0.9.1](https://github.com/esl/amoc/compare/0.9.0...0.9.1) - 2018-04-09

### Changed:
- esl/escalus updated to esl/escalus@47848b5

### Added:
- `iproute2` pkg to amoc's docker
- ability to pass graphite's port number to amoc's docker container


## [0.9.0] - 2017-01-09

### This release includes:

- Core functionality of parallel scenario execution (in either local or distributed environment)
- Possibility of controlling the scenario during execution (via the Erlang API)
- Basic instrumentation with Graphite metrics and annotations (via exometer reporter)
- An [HTTP API](https://github.com/esl/amoc/blob/master/REST_API_DOCS.md) for controlling scenario execution remotely
- Ansible automation for deploying and configuring the release on many nodes
- Set of Docker files in order to facilitate automation, but without Ansible
- Example XMPP and HTTP scenarios
- Very basic documentation
