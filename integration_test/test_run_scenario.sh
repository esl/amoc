#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

run_scenario() {
    local json_body='{ "users": '"$3"' , "settings" : { "test" : "<<\"test_value\">>" } }'
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' -d "$json_body" "$1/scenarios/$2"
}

result="$(run_scenario "http://localhost:8081" dummy_scenario 3)"

if echo ${result} | grep -q started ; then
    echo "Scenario executed"
    exit 0
else
    echo "Scenario failed"
    exit -1
fi