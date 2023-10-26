#!/bin/bash

###############################
## general purpose functions ##
###############################
function enable_strict_mode() {
    # the below settings are based on:
    #    http://redsymbol.net/articles/unofficial-bash-strict-mode/
    set -euo pipefail
    IFS=$'\n\t'
}

function compile_file() {
    local erl_file="$(realpath "$1")"
    local output_dir="$(dirname "$erl_file")"
    erlc -o "$output_dir" "$erl_file"
}

function contains() {
    local pipeline='cat -'
    for pattern in "$@"; do
        pipeline+=" | tee >(grep -q -e \"$pattern\"; echo \"+\$?\")"
    done
    pipeline+=' >/dev/null'
    local output="$(eval "$pipeline")"
    test "$(($output))" -eq 0
}

function doesnt_contain() {
    local pipeline='cat -'
    for pattern in "$@"; do
        pipeline+=" | tee >(grep -q -v -e \"$pattern\"; echo \"+\$?\")"
    done
    pipeline+=' >/dev/null'
    local output="$(eval "$pipeline")"
    test "$(($output))" -eq 0
}

function wait_for_cmd() {
    local timeout="${1:-0}"
    local cmd="${2:-true}"
    shift 2
    local full_cmd=("$cmd" "$@")
    echo "Waiting for '${full_cmd[@]}'"
    for i in $(seq 0 "${timeout}"); do
        if "${full_cmd[@]}"; then
            [ "$i" -ne 0 ] && echo
            echo "Waiting is done after $i seconds"
            return 0
        fi
        echo -n "."
        sleep 1
    done
    echo -e "\nKilled by timeout"
    return 1
}

######################
## docker functions ##
######################
docker_compose() {
    local compose_file="${git_root}/integration_test/docker-compose.yml"
    docker compose -p "amoc-test-cluster" -f "$compose_file" "$@"
}

function amoc_eval() {
    local exec_path="amoc"
    local service="$1"
    shift 1
    docker_compose exec -T "$service" "$exec_path" eval "$@"
}

function container_is_healthy() {
   docker_compose ps $1 | contains "healthy"
}

function wait_for_healthcheck() {
    local container=$1
    wait_for_cmd 60 container_is_healthy "$container"
}

######################
## common variables ##
######################
git_root="$(git rev-parse --show-toplevel)"
