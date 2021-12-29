#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

docker_compose up -d amoc-worker-3
wait_for_healthcheck amoc-worker-3

amoc_eval amoc-worker-3 "amoc_controller:get_status()." | contain dummy_scenario running
amoc_eval amoc-worker-3 "binary_to_list(amoc_config:get(test))." | contain "test_value"
amoc_eval amoc-worker-3 "dummy_helper:test_amoc_dist()." | contain 'amoc_dist_works_as_expected'
