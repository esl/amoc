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

function create_code_path() {
    cd "${git_root}/integration_test"
    dir="tmp/${1}"
    erl_file="${dir}/${1}.erl"
    mkdir -p "${dir}"
    sed "s/-module(.*)./-module(${1})./" dummy_scenario.erl > "${erl_file}"
    erlc -o "${dir}" "${erl_file}"
    cd -
}

function contain() {
    local pipeline='cat -'
    for pattern in "$@"; do
        pipeline+=" | tee >(grep -q -e \"$pattern\"; echo \"+\$?\")"
    done
    pipeline+=' >/dev/null'
    local output="$(eval "$pipeline")"
    test "$(($output))" -eq 0
}

######################
## docker functions ##
######################
function amoc_container_port() {
    local container="$1"
    if [[ "$container" =~ ^amoc-[0-9]$ ]]; then
        echo "808${container#amoc-}"
    else
        return 1
    fi
}

function start_amoc_container() {
    local name="$1"
    shift 1
    local port="$(amoc_container_port "$name")"
    docker run --rm -t -d --name "$name" -h "$name" \
        --network "$docker_network" \
        --health-cmd="/home/amoc/amoc/bin/amoc status" \
        -p "$port:4000" \
        "$@" \
        amoc:latest
}

function amoc_eval() {
    local exec_path="/home/amoc/amoc/bin/amoc"
    local container="$1"
    shift 1
    docker exec -it "$container" "$exec_path" eval "$@"
}

function get_health_status() {
    docker inspect --format '{{json .State.Health.Status }}' "$1"
}

function wait_for_healthcheck() {
    local container=$1
    local timeout="${2:-60}"
    for i in $(seq 0 "${timeout}"); do
        if [ "$(get_health_status "$container")" = "\"healthy\"" ]; then
            echo -e "\nWaiting is done after $i seconds"
            return 0
        fi
        echo -n "."
        sleep 1
    done
    echo -e "\nKilled by timeout"
    return 1
}

######################
## common variables ##
######################
git_root="$(git rev-parse --show-toplevel)"
docker_network=amoc-test-network
