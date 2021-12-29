## Integration tests

All shell scripts should conform to this
[code style](https://google.github.io/styleguide/shellguide.html)

### 1. Build amoc docker image

In the Amoc repo root directory run:

`./integration_test/build_docker_image.sh`

This command builds docker image `amoc:latest`.

### 2. Start amoc demo cluster

`./integration_test/start_demo_cluster.sh`

More information about the demo cluster can be found further in this document.

### 3. Check the amoc clustering is done properly

`./integration_test/test_amoc_cluster.sh`

This command verifies that clustering is done properly and metrics are reported
from all the nodes.

### 4. Test installation of user scenario in amoc cluster

`./integration_test/test_distribute_scenario.sh`
 
This command uploads a sample `dummy_scenario.erl` on the `amoc-master` node
using `curl` and after uploading verifies that scenario is propagated to the
worker nodes.

### 5. Run the uploaded scenario.

`./integration_test/test_run_scenario.sh`

This command starts execution of `dummy_scenario.erl` scenario (must be uploaded
before this action)

### 6. Add additional node to the cluster

`./integration_test/test_add_new_node.sh`

This command verifies that joining of the new node to the cluster is done properly.
It is expected that cluster is running `dummy_scenario.erl` scenario at the moment
when the new amoc node joins.

### 7. Cleanup

To stop Amoc demo cluster run:

`./integration_test/stop_demo_cluster.sh`

## Demo cluster

To start the demo cluster you can run these commands:

```
./integration_test/build_docker_image.sh
./integration_test/start_demo_cluster.sh
```

When the demo cluster is up and running you can access its
different components using the following addresses:
 * Amoc Swagger UI:
    * [amoc-master](http://localhost:4000/api-docs/)
    * [amoc-worker-1](http://localhost:4001/api-docs/)
    * [amoc-worker-2](http://localhost:4002/api-docs/)
 * [graphite](http://localhost:8080/) web interface
 * [grafana](http://localhost:3000/) - default username and password is `admin`/`admin`

To check the most recent `amoc-master` logs you can run this command:

`docker-compose -p "amoc-demo-cluster" logs --tail=100 amoc-master`

To attach to `amoc-master` node use the following command:

`docker-compose -p "amoc-demo-cluster" exec amoc-master /home/amoc/amoc/bin/amoc remote_console`
