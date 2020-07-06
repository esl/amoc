#!/bin/bash

# the below settings are based on:
#    http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

git_root="$(git rev-parse --show-toplevel)"
cd "${git_root}/integration_test"

# The purpose of this test is to check if scenarios installed on
# one node of Amoc cluster are distributed to all nodes

# First run test_docker_image.sh to start Amoc cluster, then run this test

port1=8081
port2=8082
port3=8083

function get_scenarios() {
    local port="$1"
    curl -s -S -H "Content-Type: application/json" -H "Accept: application/json" \
         --request GET "http://localhost:${port}/scenarios"
}

function list_scenarios_by_port() {
    local port="$1"
    ensure_scenarios_installed "$port"
}

function ensure_scenarios_installed() {
    local port="$1"
    local result="$(get_scenarios "$port")"
    echo "Scenarios on the node with port: ${port}: ${result}"

    shift 1
    local scenario_name
    for scenario_name in "$@"; do
        if echo ${result} | grep -q ${scenario_name} ; then
            echo "Scenario '${scenario_name}' installed"
            continue
        else
            echo "Scenario '${scenario_name}' not installed"
            return -1
        fi
    done
}

function upload_module() {
    local port="$1"
    local filename="$2"
    curl -s -H "Content-Type: text/plain" -T "$filename" \
         "http://localhost:${port1}/scenarios/upload"
}

scenario_name="dummy_scenario"

list_scenarios_by_port ${port1}
list_scenarios_by_port ${port2}
list_scenarios_by_port ${port3}

echo "Installing scenario and heper module on the amoc-1 node (port ${port1})"
scenario_put="$(upload_module "$port1" "dummy_scenario.erl")"
echo "Response: ${scenario_put}"
helper_put="$(upload_module "$port1" "dummy_helper.erl")"
echo "Response: ${helper_put}"

ensure_scenarios_installed ${port2} ${scenario_name}
ensure_scenarios_installed ${port3} ${scenario_name}
