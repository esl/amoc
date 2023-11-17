#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

docker_compose up -d amoc-worker-3
wait_for_healthcheck amoc-worker-3

amoc_eval amoc-worker-3 "amoc_controller:get_status()." | contains dummy_scenario running
amoc_eval amoc-worker-3 "binary_to_list(amoc_config:get(test))." | contains "test_value"
amoc_eval amoc-worker-3 "dummy_helper:test_amoc_dist()." | contains 'amoc_dist_works_as_expected'
echo "amoc_dist_works_as_expected"
