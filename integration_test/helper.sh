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
    dir="${git_root}/integration_test/extra_code_paths/${1}"
    [ -d "$dir" ] || return 1
    erl_file="${dir}/${1}.erl"
    dummy_scenario="${git_root}/integration_test/dummy_scenario.erl"
    sed "s/-module(.*)./-module(${1})./" "$dummy_scenario" > "$erl_file"
    erlc -o "$dir" "$erl_file"
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
function amoc_container_port() {
    local service="$1"
    case "$service" in
       amoc-master)
         echo 4000 ;;
       amoc-worker-[0-9])
         echo "400${service#amoc-worker-}" ;;
       *) 
         return 1 ;;
    esac
}

docker_compose() {
    local compose_file="${git_root}/integration_test/docker-compose.yml"
    docker-compose -p "amoc-demo-cluster" -f "$compose_file" "$@"
}

function amoc_eval() {
    local exec_path="/home/amoc/amoc/bin/amoc"
    local service="$1"
    shift 1
    docker_compose exec -T "$service" "$exec_path" eval "$@"
}

function container_is_healthy() {
   docker_compose ps $1 | contain "healthy"
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

######################
## common variables ##
######################
git_root="$(git rev-parse --show-toplevel)"
