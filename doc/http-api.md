AMOC REST API
==

Overview
--

Our current API allows us to:
* Control scenario execution
* List all the AMOC nodes in the cluster
* Check the status of the AMOC application (on all the nodes in the cluster)
* Upload a new scenario (or helper module)
* List available scenarios

With default options API will be running on port 4000. You can set other port by OS environment variable (`AMOC_API_PORT`).
In Amoc we use Swagger UI so if you want the current documentation in a nice format you can find it under `/api-docs/` path.
Just open it in your browser (e.g. http://localhost:4000/api-docs/)

You can also find the current documentation [here](https://esl.github.io/amoc_rest/?v=1.1.1)
(without possibility to execute requests)
