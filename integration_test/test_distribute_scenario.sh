#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

cd `dirname "$0"`/../

# The purpose of this test is to check if scenario installed on
# one node of Amoc cluster will be distributed to all nodes

# First run test_docker_image.sh to start two containers with Amoc, then run this test

get_scenarios() {
    curl -s -S -H "Content-Type: application/json" -H "Accept: application/json" --request GET http://localhost:${1}/scenarios
}

list_scenarios_by_port () {
    local PORT=$1
    local RESULT=`get_scenarios "$PORT"`
    echo "Scenarios on node with port: ${PORT}: ${RESULT}"
}

ensure_scenarios_installed () {
    local PORT=$1
    shift 1
    local SCENARIO_NAME
    local RESULT=`get_scenarios "$PORT"`
    echo "Scenarios on node with port: ${PORT}: ${RESULT}"
    for SCENARIO_NAME in "$@"; do
        if echo ${RESULT} | grep -q ${SCENARIO_NAME} ; then
            echo "Scenario '${SCENARIO_NAME}' installed"
            continue
        else
            echo "Scenario '${SCENARIO_NAME}' not installed"
            return -1
        fi
    done
}

PORT1=8081
PORT2=8082
PORT3=8083


SCENARIO_NAME="dummy_scenario"

list_scenarios_by_port ${PORT1}
list_scenarios_by_port ${PORT2}
list_scenarios_by_port ${PORT3}

echo "Installing scenario: 'dummy_scenario.erl' on node amoc-1 (port ${PORT1})"

SCEN_PUT=$(curl -s -H "Content-Type: text/plain" \
                 -T integration_test/dummy_scenario.erl \
                 'http://localhost:8081/upload')
echo "Response: ${SCEN_PUT}"

ensure_scenarios_installed ${PORT2} ${SCENARIO_NAME}
ensure_scenarios_installed ${PORT3} ${SCENARIO_NAME}

