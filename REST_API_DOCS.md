AMOC REST API
==

Overview
--

Current our API allows us to:
* Start/stop a scenario
* Checking liveness of AMOC
* Upload scenario
* List available scenarios
* Pinging every AMOC nodes from master node
* Get status of running scenario on single node

With default options API will be running on port 4000. You can set other port by OS environment variable (`AMOC_api_port`), or `application:set_env(amoc, api_port, PORT_NUMBER)`.
Every request need to have a header field `content-type: application/json`.

Attention
---
In project we use Swagger so if you want current documentation in very nice format you can do `GET /api-docs`.
Requests and responses format
---

### List scenarios

#### Request
`GET /scenarios`
#### Response
`
{
    "scenarios": [ScenarioName]
}
`
### Add new scenario

#### Request
`POST /scenarios` with:
`
{
    "scenario": ScenarioName, //string
    "module_source": ModuleSourceCode //string
}
`
#### Response
`
{
    "compile" : "ok" | "error"
}
`
### Start scenario

#### Request 
`PATCH /scenarios/:id` with:
`
{
    "users": NumberOfUsers //integer
}
`
#### Response
`
{
    "scenario": "started" | "wrong_json"
}
`
### Scenario status

#### Request
`GET /scenarios/:id`
#### Response
`{
    "scenario_status": "loaded" | "running" | "finished"
}`
Attention: when scenario with `:id` does not exists API returns 404.
### Ping nodes

#### Request
`GET /nodes`
#### Response
`{
    "nodes": 
    {
        NodeName1: "up" | "down",
        NodeName2: "up" | "down",
        ...
    }
}`
### Node status

#### Request
`GET /status`
#### Response
`{
    "node_status": "up" | "down"
}`
