#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

compile_file integration_test/extra_code_paths/path1/dummy_helper.erl
compile_file integration_test/extra_code_paths/path2/dummy_scenario.erl

docker_compose up -d amoc-{master,worker-1,worker-2}

wait_for_healthcheck amoc-master
wait_for_healthcheck amoc-worker-1
wait_for_healthcheck amoc-worker-2
