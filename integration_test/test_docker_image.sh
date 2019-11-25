#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/

cd `dirname "$0"`

set -euo pipefail
IFS=$'\n\t'

./create_code_path.sh test1
./create_code_path.sh test2

NETWORK=amoc-test-network
PATH_TO_EXEC=/home/amoc/amoc/bin/amoc
docker network create ${NETWORK}

AMOC_NODES="['amoc@amoc-1','amoc@amoc-2']"

docker run --rm -t -d --name amoc-1 -h amoc-1 \
    --network ${NETWORK} \
    -v "${PWD}/tmp:/test:ro" \
    -e AMOC_NODES=${AMOC_NODES} \
    -e AMOC_EXTRA_CODE_PATHS='["/test/test1", "/test/test2"]' \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8081:4000 \
    amoc:latest

docker run --rm -t -d --name amoc-2 -h amoc-2 \
    --network ${NETWORK} \
    -v "${PWD}/tmp:/test:ro" \
    -e AMOC_NODES=${AMOC_NODES} \
    -e AMOC_EXTRA_CODE_PATHS='["/test/test1", "/test/test2"]' \
        --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8082:4000 \
    amoc:latest

./wait_for_healthcheck.sh amoc-1
./wait_for_healthcheck.sh amoc-2

docker exec -it amoc-1 ${PATH_TO_EXEC} eval "nodes()" | grep amoc-2
docker exec -it amoc-2 ${PATH_TO_EXEC} eval "nodes()" | grep amoc-1


