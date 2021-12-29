#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

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

list_scenarios_by_port amoc-master
list_scenarios_by_port amoc-worker-1
list_scenarios_by_port amoc-worker-2

echo "Installing scenario and helper module on the amoc-master node"
scenario_put="$(upload_module amoc-master "${scenario_name}.erl")"
echo "Response for '${scenario_name}.erl': ${scenario_put}"
helper_put="$(upload_module amoc-master "dummy_helper.erl")"
echo "Response for 'dummy_helper.erl': ${helper_put}"

ensure_scenarios_installed amoc-worker-1 ${scenario_name}
ensure_scenarios_installed amoc-worker-2 ${scenario_name}
