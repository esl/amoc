#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

docker_compose down
