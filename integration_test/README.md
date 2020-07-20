## Integration tests

All shell scripts should conform to this
[code style](https://google.github.io/styleguide/shellguide.html)

### 1. Build docker containers

In amoc dirctory run:

`./integration_test/build_docker_image.sh`

This will build docker image `amoc:latest`.

### 2. Run docker containers
`./integration_test/test_docker_image.sh`

This will start two containers `amoc-1` and `amoc-2` and
will wait until healthcheck is successful.

### 3. Test installation of user scenario in amoc cluster

`./integration_test/test_distribute_scenario.sh`
 
This will install a sample `dummy_scenario.erl` on the `amoc-1` node
using curl and then it will verify that it is propagated to the other
node in the cluster, `amoc-2`.

### 4. Run the scenario and verify that it finishes without errors

`./integration_test/test_run_scenario.sh`

### 5. Cleanup

To stop containers and remove Amoc image run:

`./integration_test/cleanup_containers.sh`

## Demo cluster

To start the demo cluster you can run these commands:

```
./integration_test/build_docker_image.sh
./integration_test/start_demo_cluster.sh
```

When the demo cluster is up and running you can access its
different components using the following addresses:
 * Amoc Swagger UI:
    * [amoc-1](http://localhost:8081/api-docs/)
    * [amoc-2](http://localhost:8082/api-docs/)
    * [amoc-3](http://localhost:8083/api-docs/)
 * [graphite](http://localhost:8080/) web interface
 * [grafana](http://localhost:3000/) - default username and password is `admin`/`admin`

To check the most recent `amoc-1` logs you can run this command:

`docker exec amoc-1 tail /home/amoc/amoc/log/erlang.log`

To attach to `amoc-1` node use the following command:

`docker exec -it amoc-1 /home/amoc/amoc/bin/amoc remote_console`
