#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

start_amoc_container amoc-4 -e AMOC_NODES="['amoc@amoc-1']"

wait_for_healthcheck amoc-4

amoc_eval amoc-4 "amoc_controller:get_status()." | contain dummy_scenario running
amoc_eval amoc-4 "amoc_config:get(test)." | contain "test_value"
amoc_eval amoc-4 "dummy_helper:test_amoc_dist()." | contain 'amoc_dist_works_as_expected'
