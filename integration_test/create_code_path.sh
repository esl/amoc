#!/bin/bash

# the below settings are based on:
#    http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

git_root="$(git rev-parse --show-toplevel)"
cd "${git_root}/integration_test"

dir="tmp/${1}"
erl_file="${dir}/${1}.erl"
mkdir -p "${dir}"
sed "s/-module(.*)./-module(${1})./" dummy_scenario.erl > "${erl_file}"
erlc -o "${dir}" "${erl_file}"
