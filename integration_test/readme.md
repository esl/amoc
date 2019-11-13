## Integration tests

### 1. Build docker containers

In amoc dirctory run:

`./integration_test/build_docker_image.sh`

This will build docker image `amoc:latest`.

### 2. Run docker containers
`./integration_test/test_docker_image.sh`

This will start two containers `amoc-1` and `amoc-2` and will wait until healthcheck is successful.

### 3. Test installation of user scenario in amoc cluster

 `./integration_test/test_distribute_scenario.sh`
 
This will install sample scenario `sample_test.erl` on node `amoc-1` using curl and then verify that scenario was propagated to the other node in the cluster `amoc-2`.

### 4. Cleanup

To stop containers and remove Amoc image run:

`./integration_test/cleanup_containers.sh`
