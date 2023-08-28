#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

echo "checking that clustering is done properly"
amoc_eval amoc-master "nodes()." | contain amoc-worker-1 amoc-worker-2
amoc_eval amoc-worker-1 "nodes()." | contain amoc-master amoc-worker-2
amoc_eval amoc-worker-2 "nodes()." | contain amoc-master amoc-worker-1

echo "checking that AMOC_EXTRA_CODE_PATHS setting works as expected"
amoc_eval amoc-master "amoc_code_server:does_scenario_exist(test1)." | contain true
amoc_eval amoc-master "amoc_code_server:does_scenario_exist(test2)." | contain true
