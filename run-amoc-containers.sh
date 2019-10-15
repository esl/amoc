#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-test-network
PATH_TO_AMOC=/home/amoc/amoc/bin/amoc

docker network create ${NETWORK}

docker run --rm -d --name=graphite --network ${NETWORK} \
    -p 2003:2003 -p 8080:80 graphiteapp/graphite-statsd

docker run --rm -t -d --name amoc-1 -h amoc-1 --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_GRAPHITE_HOST=graphite \
    -e AMOC_GRAPHITE_PORT=2003 \
    -e AMOC_PREFIX=amoc1 \
    --health-cmd="${PATH_TO_AMOC} status" \
    -p 8081:4000 \
    amoc-reworked:latest

docker run --rm -t -d --name amoc-2 -h amoc-2 --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_GRAPHITE_HOST=graphite \
    -e AMOC_GRAPHITE_PORT=2003 \
    -e AMOC_PREFIX=amoc2 \
    --health-cmd="${PATH_TO_AMOC} status" \
    -p 8082:4000 \
    amoc-reworked:latest
