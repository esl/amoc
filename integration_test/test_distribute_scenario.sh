#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

# The purpose of this test is to check if scenarios installed on
# one node of Amoc cluster are distributed to all nodes

# First run test_docker_image.sh to start Amoc cluster, then run this test
scenario_name="dummy_scenario"

#############################
## amoc REST API functions ##
#############################
function get_scenarios() {
    local port="$(amoc_container_port "$1")"
    curl -s -S -H "Content-Type: application/json" -H "Accept: application/json" \
         --request GET "http://localhost:${port}/scenarios"
}

function list_scenarios_by_port() {
    local result="$(get_scenarios "$1")"
    echo "Scenarios on the ${1} node: ${result}"
}

function ensure_scenarios_installed() {
    local result="$(get_scenarios "$1")"
    echo "Scenarios on the ${1} node: ${result}"
    shift 1
    echo "$result" | contain "$@"
}

function upload_module() {
    local port="$(amoc_container_port "$1")"
    local filename="$2"
    curl -s -H "Content-Type: text/plain" -T "$filename" \
         "http://localhost:${port}/scenarios/upload"
}

list_scenarios_by_port amoc-1
list_scenarios_by_port amoc-2
list_scenarios_by_port amoc-3

echo "Installing scenario and heper module on the amoc-1 node"
scenario_put="$(upload_module amoc-1 "${scenario_name}.erl")"
echo "Response: ${scenario_put}"
helper_put="$(upload_module amoc-1 "dummy_helper.erl")"
echo "Response: ${helper_put}"

ensure_scenarios_installed amoc-2 ${scenario_name}
ensure_scenarios_installed amoc-3 ${scenario_name}
