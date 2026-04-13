#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

docker_compose up --wait --wait-timeout 100 amoc-worker-3

amoc_eval amoc-worker-3 "amoc_controller:get_status()." | contains_all dummy_scenario running


amoc_eval amoc-worker-3 "binary_to_list(amoc_config:get(global_test1))." | contains_all "test_value1"
amoc_eval amoc-worker-3 "binary_to_list(amoc_config:get(global_test2))." | contains_all "test_value3"
amoc_eval amoc-worker-3 "binary_to_list(amoc_config:get(test1))." | contains_all "test_value2"
amoc_eval amoc-worker-3 "atom_to_list(amoc_config:get(test2))." | contains_all "undefined"

amoc_eval amoc-worker-3 "dummy_helper:test_amoc_dist()." | contains_all 'amoc_dist_works_as_expected'
echo "amoc_dist_works_as_expected"
