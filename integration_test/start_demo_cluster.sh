#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "${git_root}/integration_test"

start_graphite_container

## start grafana
docker run --rm -d --name=grafana \
           -p 3000:3000 --network="$docker_network" \
           grafana/grafana:6.7.3

amoc_nodes="['amoc@amoc-1']"

start_amoc_container amoc-1 -e AMOC_NODES="${amoc_nodes}"
start_amoc_container amoc-2 -e AMOC_NODES="${amoc_nodes}"
start_amoc_container amoc-3 -e AMOC_NODES="${amoc_nodes}"

wait_for_healthcheck amoc-1
wait_for_healthcheck amoc-2
wait_for_healthcheck amoc-3

json=( '{'
       '"name": "graphite",'
       '"access": "proxy",'
       '"type": "graphite",'
       '"url": "http://graphite",'
       '"isDefault": true'
       '}' )

## configure default grafana datasource
curl 'http://admin:admin@localhost:3000/api/datasources' -X POST \
     -H 'Content-Type: application/json;charset=UTF-8' \
     -d "${json[*]}" -w "\n%{response_code}\n"
