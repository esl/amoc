AMOC REST API
==

Overview
--

Current our API allows us to:
* Start a scenario
* Checking liveness of AMOC erlang application
* Upload scenario
* List available scenarios
* Pinging every AMOC nodes from master node
* Get status of running scenario on single node

With default options API will be running on port 4000. You can set other port by OS environment variable (`AMOC_API_PORT`).
In Amoc we use Swagger so if you want the current documentation in a nice format you can find it under `/api-docs/` path.
Just open it in your browser (e.g. http://localhost:4000/api-docs/)

Also you can find the current documentation [here](https://esl.github.io/amoc_rest/?v=1.0.0)
(without possibility to execute requests)
