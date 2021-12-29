#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################
run_scenario() {
    local port="$(amoc_container_port "$1")"
    local json_body='{ "scenario": "'"$2"'", "users": '"$3"' , "settings" : { "test" : "<<\"test_value\">>" } }'
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' \
         -s  -w "%{http_code}" -o /dev/null -d "$json_body" "http://localhost:${port}/execution/start"
}

result="$(run_scenario amoc-master dummy_scenario 10)"

if [ "$result" = "200" ]; then
    echo "Scenario executed"
    exit 0
else
    echo "Scenario failed"
    exit -1
fi
