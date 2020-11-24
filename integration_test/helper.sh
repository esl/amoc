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
function start_graphite_container() {
    docker network create "${docker_network}"
    docker run --rm -d --name=graphite \
               -p 2003:2003 -p 8080:80 \
               --network="$docker_network" \
               graphiteapp/graphite-statsd:1.1.7-2
}

function amoc_container_port() {
    local container="$1"
    if [[ "$container" =~ ^amoc-[0-9]$ ]]; then
        echo "888${container#amoc-}"
    else
        return 1
    fi
}

function start_amoc_container() {
    local name="$1"
    shift 1
    local port="$(amoc_container_port "$name")"
    docker run --rm -d --name "$name" -h "$name" \
               --network "$docker_network" \
               -e AMOC_GRAPHITE_HOST='"graphite"' \
               --health-cmd="/home/amoc/amoc/bin/amoc status" \
               -p "$port:4000" \
               "$@" \
               amoc:latest
}

function amoc_eval() {
    local exec_path="/home/amoc/amoc/bin/amoc"
    local container="$1"
    shift 1
    docker exec "$container" "$exec_path" eval "$@"
}

function get_health_status() {
    docker inspect --format '{{json .State.Health.Status }}' "$1"
}

function container_is_healthy() {
  local health_status="$(get_health_status "$1")"
  #echo "$1 container health status == '${health_status}'"
  [ "$health_status" = "\"healthy\"" ]
}

function wait_for_healthcheck() {
    local container=$1
    wait_for_cmd 60 container_is_healthy "$container"
}

function metrics_reported() {
    local graphite_query="target=summarize(*.amoc.users.size,'1hour','max')&from=-1h&format=json"
    local result="$(curl -s "http://localhost:8080/render/?${graphite_query}")"
    echo "$result" | contain "$@"
}

function wait_for_metrics() {
     wait_for_cmd 60 metrics_reported "$@"
}

function wait_for_cmd() {
    local timeout="${1:-0}"
    local cmd="${2:-true}"
    shift 2
    local full_cmd=("$cmd" "$@")
    echo "Waiting for '${full_cmd[@]}'"
    for i in $(seq 0 "${timeout}"); do
        if "${full_cmd[@]}"; then
            echo -e "\nWaiting is done after $i seconds"
            return 0
        fi
        echo -n "."
        sleep 1
    done
    echo -e "\nKilled by timeout"
    return 1
}

function get_amoc_logs() {
    local logs_path="/home/amoc/amoc/log/erlang.log"
    local container="$1"
    docker exec "$container" cat "$logs_path"
}

######################
## common variables ##
######################
git_root="$(git rev-parse --show-toplevel)"
docker_network=amoc-test-network
