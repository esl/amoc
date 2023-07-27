#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

scenario_name="dummy_scenario"

#############################
## amoc REST API functions ##
#############################
function get_scenarios() {
    amoc_eval "$1" "amoc_scenario:list_scenario_modules()."
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
    local filename="$2"
    docker_compose cp "$2" "${1}:/tmp/${2}"
    eval_cmd=( "File = \"/tmp/${2%.erl}\","
               '{ok, Module} = compile:file(File, [{outdir,"/tmp"}]),'
               'true = code:add_path("/tmp"),' ## required for successful amoc_scenario:add_module/1 execution
               '{module, Module} = code:load_file(Module),'
               'amoc_scenario:add_module(Module).' )
    echo "${eval_cmd[*]}"
    amoc_eval "${1}" "${eval_cmd[*]}"
}

list_scenarios_by_port amoc-master
list_scenarios_by_port amoc-worker-1
list_scenarios_by_port amoc-worker-2

echo "Installing scenario and helper module on the amoc-master node"
scenario_put="$(upload_module amoc-master "${scenario_name}.erl")"
echo "Response for '${scenario_name}.erl': ${scenario_put}"
helper_put="$(upload_module amoc-master "dummy_helper.erl")"
echo "Response for 'dummy_helper.erl': ${helper_put}"

# ensure_scenarios_installed amoc-worker-1 ${scenario_name}
# ensure_scenarios_installed amoc-worker-2 ${scenario_name}
