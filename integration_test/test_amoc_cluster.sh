#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

echo "checking that clustering is done properly"
amoc_eval amoc-master "nodes()." | contains_all amoc-worker-1 amoc-worker-2
amoc_eval amoc-worker-1 "nodes()." | contains_all amoc-master amoc-worker-2
amoc_eval amoc-worker-2 "nodes()." | contains_all amoc-master amoc-worker-1

echo "checking that setting AMOC_EXTRA_CODE_PATHS env works as expected"
amoc_eval amoc-master "amoc_code_server:list_scenario_modules()." | contains_all dummy_scenario
amoc_eval amoc-master "amoc_code_server:list_configurable_modules()." | contains_all dummy_helper
amoc_eval amoc-worker-1 "amoc_code_server:list_scenario_modules()." | doesnt_contain_any dummy_scenario
amoc_eval amoc-worker-1 "amoc_code_server:list_configurable_modules()." | doesnt_contain_any dummy_helper
amoc_eval amoc-worker-2 "amoc_code_server:list_scenario_modules()." | doesnt_contain_any dummy_scenario
amoc_eval amoc-worker-2 "amoc_code_server:list_configurable_modules()." | doesnt_contain_any dummy_helper
