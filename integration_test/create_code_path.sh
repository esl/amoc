#!/bin/bash

cd `dirname "$0"`

dir="tmp/${1}"
erl_file="${dir}/${1}.erl"
mkdir -p "${dir}"
sed "s/-module(.*)./-module(${1})./" dummy_scenario.erl > "${erl_file}"
erlc -o "${dir}" "${erl_file}"
