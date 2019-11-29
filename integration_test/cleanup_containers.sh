#!/bin/bash

docker stop amoc-1 amoc-2 amoc-3
docker network rm amoc-test-network

echo "Amoc containers removed"
