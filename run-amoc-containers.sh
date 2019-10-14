#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-test-network
PATH_TO_AMOC=/home/amoc/amoc/bin/amoc
PATH_TO_MONGOOSE=/usr/lib/mongooseim/bin/mongooseimctl

docker network create ${NETWORK}

docker run -d -t -h mongooseim-1 --name mongooseim-1 --network ${NETWORK} \
    -p 5222:5222 \
    --health-cmd="${PATH_TO_MONGOOSE} status" \
    mongooseim/mongooseim:latest

docker run -t -d --name amoc-1 -h amoc-1 --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_xmpp_servers="[[{host, \"mongooseim-1\"}, {port, 5222}]]" \
    --health-cmd="${PATH_TO_AMOC} status" \
    amoc-reworked:latest

docker run -t -d --name amoc-2 -h amoc-2 --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_xmpp_servers="[[{host, \"mongooseim-1\"}, {port, 5222}]]" \
    --health-cmd="${PATH_TO_AMOC} status" \
    amoc-reworked:latest
