#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

create_code_path test1
create_code_path test2

docker_compose up -d amoc-{master,worker-1,worker-2}

wait_for_healthcheck amoc-master
wait_for_healthcheck amoc-worker-1
wait_for_healthcheck amoc-worker-2
