#!/bin/bash

# the below settings are based on:
#    http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

git_root="$(git rev-parse --show-toplevel)"
cd "${git_root}/integration_test"

./create_code_path.sh test1
./create_code_path.sh test2

function docker_eval() {
    local exec_path="/home/amoc/amoc/bin/amoc"
    local container="$1"
    shift 1
    docker exec -it "$container" "$exec_path" eval "$@"
}

network=amoc-test-network
docker network create "${network}"

amoc_nodes="['amoc@amoc-1','amoc@amoc-2']"

docker run --rm -t -d --name amoc-1 -h amoc-1 \
    --network ${network} \
    -v "${PWD}/tmp:/test:ro" \
    -e AMOC_NODES=${amoc_nodes} \
    -e AMOC_EXTRA_CODE_PATHS='["/test/test1", "/test/test2"]' \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8081:4000 \
    amoc:latest

docker run --rm -t -d --name amoc-2 -h amoc-2 \
    --network ${network} \
    -e AMOC_NODES=${amoc_nodes} \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8082:4000 \
    amoc:latest

docker run --rm -t -d --name amoc-3 -h amoc-3 \
    --network ${network} \
    -e AMOC_NODES=${amoc_nodes} \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8083:4000 \
    amoc:latest

./wait_for_healthcheck.sh amoc-1
./wait_for_healthcheck.sh amoc-2
./wait_for_healthcheck.sh amoc-3

docker_eval amoc-1 "nodes()" | grep amoc-2 | grep amoc-3
docker_eval amoc-2 "nodes()" | grep amoc-1 | grep amoc-3
docker_eval amoc-3 "nodes()"  | grep amoc-1 | grep amoc-2

docker_eval amoc-1 "amoc_scenario:does_scenario_exist(test1)" | grep true
docker_eval amoc-1 "amoc_scenario:does_scenario_exist(test2)" | grep true
