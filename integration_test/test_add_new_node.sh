#!/bin/bash

# the below settings are based on:
#    http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

git_root="$(git rev-parse --show-toplevel)"
cd "${git_root}/integration_test"

network=amoc-test-network

function docker_eval() {
    local exec_path="/home/amoc/amoc/bin/amoc"
    docker exec -it amoc-4 "$exec_path" eval "$@"
}

docker run --rm -t -d --name amoc-4 -h amoc-4 \
    --network "$network" \
    -e AMOC_NODES="['amoc@amoc-1']" \
    --health-cmd="/home/amoc/amoc/bin/amoc status" \
    -p 8084:4000 \
    amoc:latest

./wait_for_healthcheck.sh amoc-4

docker_eval "amoc_controller:get_status()" | grep dummy_scenario | grep running
docker_eval "amoc_config:get(test)" | grep '<<"test_value">>'
docker_eval "dummy_helper:test_amoc_dist()" | tee /dev/tty | grep -q 'amoc_dist_works_as_expected'
