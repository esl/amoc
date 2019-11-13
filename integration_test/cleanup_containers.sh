#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

docker stop amoc-1 amoc-2
docker network rm amoc-test-network
docker image rm amoc

echo "Amoc containers and image removed"
