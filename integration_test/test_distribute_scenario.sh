#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

scenario_name="dummy_scenario"

#############################
## amoc REST API functions ##
#############################
function get_scenarios() {
    amoc_eval "$1" "amoc_code_server:list_scenario_modules()."
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

function add_module() {
    echo "distributing module '${2}' from the node '${1}'"
    amoc_eval "${1}" "amoc_code_server:add_module($2)."
}

list_scenarios_by_port amoc-master
list_scenarios_by_port amoc-worker-1
list_scenarios_by_port amoc-worker-2

echo "Distributing scenario and helper module from the amoc-master node"
add_module amoc-master "$scenario_name"
add_module amoc-master "dummy_helper"

# ensure_scenarios_installed amoc-worker-1 ${scenario_name}
# ensure_scenarios_installed amoc-worker-2 ${scenario_name}
