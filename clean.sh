#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-test-network
PATH_TO_EXEC=/home/amoc/amoc/bin/amoc
docker container rm -f amoc-1
docker container rm -f amoc-2
docker container rm -f mongooseim-1
docker network rm ${NETWORK}


