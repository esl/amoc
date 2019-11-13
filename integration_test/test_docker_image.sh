#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-test-network
PATH_TO_EXEC=/home/amoc/amoc/bin/amoc
docker network create ${NETWORK}

docker run --rm -t -d --name amoc-1 -h amoc-1 \
    --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_PREFIX=amoc1 \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8081:4000 \
    amoc:latest

docker run --rm -t -d --name amoc-2 -h amoc-2 \
    --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_PREFIX=amoc2 \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8082:4000 \
    amoc:latest

./integration_test/wait_for_healthcheck.sh amoc-1
./integration_test/wait_for_healthcheck.sh amoc-2

docker exec -it amoc-1 ${PATH_TO_EXEC} eval "nodes()" | grep amoc-2
docker exec -it amoc-2 ${PATH_TO_EXEC} eval "nodes()" | grep amoc-1


