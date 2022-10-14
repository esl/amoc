#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################
run_scenario() {
    amoc_eval "$1" "amoc_dist:do(${2}, ${3})."
}

result="$(run_scenario amoc-master dummy_scenario 10)"

echo "$result"

if echo "$result" | contain "ok" "'amoc@amoc-worker-1'" "'amoc@amoc-worker-2'" ; then
    echo "Scenario executed"
    exit 0
else
    echo "Scenario failed"
    exit -1
fi
