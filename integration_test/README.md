## Integration tests

All shell scripts should conform to this
[code style](https://google.github.io/styleguide/shellguide.html)

### 1. Build amoc docker image

In the Amoc repo root directory run:

`./integration_test/build_docker_image.sh`

This command builds the `amoc:latest` docker image.

### 2. Start amoc test cluster

`./integration_test/start_test_cluster.sh`

This command requires the `amoc:latest` docker image to exist. More information about the test cluster can be found further in this document.

### 3. Check that clustering is done properly

`./integration_test/test_amoc_cluster.sh`

This command verifies that clustering is done properly.

### 4. Test distribution of a custom scenario in amoc cluster

`./integration_test/test_distribute_scenario.sh`

This command checks distribution of the sample `dummy_scenario.erl` from the `amoc-master` node
 to the worker nodes.

### 5. Run the distributed scenario.

`./integration_test/test_run_scenario.sh`

This command starts execution of `dummy_scenario.erl` scenario (it must be distributed
prior to this action)

### 6. Add additional node to the cluster

`./integration_test/test_add_new_node.sh`

This command verifies that joining of the new node to the cluster is done properly.
It is expected that cluster is running `dummy_scenario.erl` scenario at the moment
when the new amoc node joins.

### 7. Cleanup

To stop Amoc test cluster run:

`./integration_test/stop_test_cluster.sh`

## Test cluster

To start the test cluster you can run these commands:

```
./integration_test/build_docker_image.sh
./integration_test/start_test_cluster.sh
```

To check the most recent `amoc-master` logs you can run this command:

`docker compose -p "amoc-test-cluster" logs --tail=100 amoc-master`

In order to attach to the `amoc-master` node use the following command:

`docker compose -p "amoc-test-cluster" exec amoc-master amoc remote_console`
