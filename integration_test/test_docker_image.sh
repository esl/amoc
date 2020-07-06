#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

create_code_path test1
create_code_path test2

docker network create "${docker_network}"

amoc_nodes="['amoc@amoc-1', 'amoc@amoc-2']"

start_amoc_container amoc-1 -e AMOC_NODES="${amoc_nodes}" \
    -e AMOC_EXTRA_CODE_PATHS='["/test/test1", "/test/test2"]'\
    -v "${PWD}/tmp:/test:ro"

start_amoc_container amoc-2 -e AMOC_NODES="${amoc_nodes}"
start_amoc_container amoc-3  -e AMOC_NODES="${amoc_nodes}"

wait_for_healthcheck amoc-1
wait_for_healthcheck amoc-2
wait_for_healthcheck amoc-3

amoc_eval amoc-1 "nodes()" | contain amoc-2 amoc-3
amoc_eval amoc-2 "nodes()" | contain amoc-1 amoc-3
amoc_eval amoc-3 "nodes()" | contain amoc-1 amoc-2

amoc_eval amoc-1 "amoc_scenario:does_scenario_exist(test1)" | contain true
amoc_eval amoc-1 "amoc_scenario:does_scenario_exist(test2)" | contain true
