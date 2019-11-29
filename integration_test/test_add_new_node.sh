#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/

cd `dirname "$0"`

set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-test-network
PATH_TO_EXEC=/home/amoc/amoc/bin/amoc


docker run --rm -t -d --name amoc-3 -h amoc-3 \
    --network ${NETWORK} \
    -e AMOC_NODES="['amoc@amoc-1']" \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8083:4000 \
    amoc:latest

./wait_for_healthcheck.sh amoc-3
docker exec -it amoc-3 ${PATH_TO_EXEC} eval "amoc_controller:get_status()" | grep dummy_scenario | grep running
docker exec -it amoc-3 ${PATH_TO_EXEC} eval "amoc_config:get(test)" | grep '<<"test_value">>'




