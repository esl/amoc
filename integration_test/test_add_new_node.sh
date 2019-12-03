#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/

cd `dirname "$0"`

set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-test-network
PATH_TO_EXEC=/home/amoc/amoc/bin/amoc


docker run --rm -t -d --name amoc-4 -h amoc-4 \
    --network ${NETWORK} \
    -e AMOC_NODES="['amoc@amoc-1']" \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8084:4000 \
    amoc:latest

./wait_for_healthcheck.sh amoc-4

docker exec -it amoc-4 ${PATH_TO_EXEC} eval "amoc_controller:get_status()" | grep dummy_scenario | grep running
docker exec -it amoc-4 ${PATH_TO_EXEC} eval "amoc_config:get(test)" | grep '<<"test_value">>'
docker exec -it amoc-4 ${PATH_TO_EXEC} eval "dummy_scenario:test_amoc_dist()" | tee /dev/tty | grep -q 'amoc_dist_works_as_expected'





