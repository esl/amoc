#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

modules=( "dummy_scenario" "dummy_helper" )

function get_scenarios() {
    amoc_eval "$1" "amoc_code_server:list_scenario_modules()."
}

function get_helpers() {
    amoc_eval "$1" "amoc_code_server:list_configurable_modules()."
}

function list_scenarios_and_helpers() {
    local scenarios="$(get_scenarios "$1")"
    local helpers="$(get_helpers "$1")"
    echo "Scenarios on the ${1} node: ${scenarios}"
    echo "Configurable helpers on the ${1} node: ${helpers}"
}

function erlang_list() {
    local ret=( "[" )
    local element
    if [ "$#" -gt 0 ]; then
        ret+=( "$1" )
        shift 1
        for element in "$@"; do
            ret+=( "," "$element" )
        done
    fi
    ret+=( "]" )
    echo "${ret[@]}"
}

function ensure_modules_loaded() {
    local node="$1"
    shift 1
    local modules="$(erlang_list "$@")"
    amoc_eval "$node" "[code:ensure_loaded(M) || M <- ${modules}]."
}

function add_module() {
    local node="${1}"
    local module
    shift 1
    for module in "$@"; do
        echo "adding module '${module}' for distribution from the node '${node}'"
        amoc_eval "${node}" "amoc_code_server:add_module(${module})."
    done
}

function distribute_modules() {
    echo "distributing modules '${1}' --> '${2}'"
    amoc_eval "${1}" "amoc_code_server:distribute_modules('amoc@${2}')."
}

ensure_modules_loaded amoc-master "${modules[@]}" | contains "${modules[@]}"
ensure_modules_loaded amoc-worker-1 "${modules[@]}" | doesnt_contain "${modules[@]}"
ensure_modules_loaded amoc-worker-2 "${modules[@]}" | doesnt_contain "${modules[@]}"

list_scenarios_and_helpers amoc-worker-2 | doesnt_contain "${modules[@]}"
list_scenarios_and_helpers amoc-worker-1 | doesnt_contain "${modules[@]}"

echo "Distributing scenario and helper module from the amoc-master node"
## amoc_controller is added to the list as an example of module
## that already exists on all the slave amoc nodes
add_module amoc-master "${modules[@]}" amoc_controller
distribute_modules amoc-master amoc-worker-1

ensure_modules_loaded amoc-worker-1 "${modules[@]}" | contains "${modules[@]}"
ensure_modules_loaded amoc-worker-2 "${modules[@]}" | doesnt_contain "${modules[@]}"

list_scenarios_and_helpers amoc-worker-1 | contains "${modules[@]}"
list_scenarios_and_helpers amoc-worker-2 | doesnt_contain "${modules[@]}"
