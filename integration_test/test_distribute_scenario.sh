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

SCENARIO_NAME="dummy_scenario"

list_scenarios_by_port ${PORT1}
list_scenarios_by_port ${PORT2}

# Use jq to convert string to safe JSON string
# see: https://stackoverflow.com/questions/10053678/escaping-characters-in-bash-for-json/13466143
# -a means "ascii output"
# -R means "raw input"
# -s means "include linebreaks"
# . means "output the root of the JSON document"
SCEN_SOURCE=`cat integration_test/dummy_scenario.erl | jq -aRs .`

echo "Installing scenario: 'dummy_scenario.erl' on node amoc-1 (port ${PORT1})"

SCEN_POST=$( (echo '{"scenario":"dummy_scenario","module_source":'; echo ${SCEN_SOURCE} ; echo '}' ) | curl \
     -s -S \
     -H "Content-Type: application/json" \
     -H "Accept: application/json" \
     --request POST \
     --data @- \
     http://localhost:8081/scenarios)
echo "Response: ${SCEN_POST}"

ensure_scenarios_installed ${PORT2} ${SCENARIO_NAME}
