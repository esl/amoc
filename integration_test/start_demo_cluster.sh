#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

create_code_path test1
create_code_path test2

docker_compose up -d amoc-{master,worker-1,worker-2} graphite grafana

wait_for_healthcheck amoc-master
wait_for_healthcheck amoc-worker-1
wait_for_healthcheck amoc-worker-2

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
