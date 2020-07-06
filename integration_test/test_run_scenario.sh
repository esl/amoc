#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################
run_scenario() {
    local port="$(amoc_container_port "$1")"
    local json_body='{ "users": '"$3"' , "settings" : { "test" : "<<\"test_value\">>" } }'
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' \
         -d "$json_body" "http://localhost:${port}/scenarios/$2"
}

result="$(run_scenario amoc-1 dummy_scenario 10)"

if echo ${result} | contain started; then
    echo "Scenario executed"
    exit 0
else
    echo "Scenario failed"
    exit -1
fi