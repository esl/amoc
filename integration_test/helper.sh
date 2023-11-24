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

function contains_all() {
    local output="$(cat -)"
    local ret= acc=0
    for pattern in "$@"; do
        ret="$(echo "$output" | grep -L -e "$pattern" | wc -l)"
        if [ "$ret" -ne "0" ]; then
            [ "$(($acc))" -eq "0" ] && echo "output: '${output}'"
            echo "pattern is missing: '${pattern}'"
        fi > /dev/tty
        acc+="+${ret}"
    done
    test "$(($acc))" -eq 0
}

function doesnt_contain_any() {
    local output="$(cat -)"
    local ret= acc=0
    for pattern in "$@"; do
        ret="$(echo "$output" | grep -l -e "$pattern" | wc -l || true)"
        if [ "$ret" -ne "0" ]; then
            [ "$(($acc))" -eq "0" ] && echo "output: '${output}'"
            echo "pattern is present: '${pattern}'"
        fi > /dev/tty
        acc+="+${ret}"
    done
    test "$(($acc))" -eq 0
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

######################
## common variables ##
######################
git_root="$(git rev-parse --show-toplevel)"
