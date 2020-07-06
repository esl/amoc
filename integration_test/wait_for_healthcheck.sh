#!/usr/bin/env bash
# Waiting for a running container to become healthy.
# Waits if no container was found with reason "no such object".
# Waits for the container configured healthcheck.
# Fails if this script runs for too long with the reason "Killed by timeout".
#
# Usage example:
# ./tools/wait_for_healthcheck.sh <container_name>
#
# TIMEOUT argument documentation
# ------------------------------
#
# TIMEOUT is number of times we try to get healthcheck status.
# Because we wait one second between tries, it can be interpreted as
# number of seconds we wait.
#
# with 5 seconds timeout:
# TIMEOUT=5 ./tools/wait_for_healthcheck.sh <container_name>
#
# If call to docker daemon gets blocked for a long time
# (for example, if docker daemon is down),
# that TIMEOUT would not work as expected.
#
# If "docker inspect" takes some time to execute, than TIMEOUT does not work
# as expected.
#
# This command would try to get healthcheck status once:
# TIMEOUT=0 ./tools/wait_for_healthcheck.sh <container_name>

set -e

if [ "$#" -ne 1 ]; then
    exit "Illegal number of parameters"
fi
container="$1"

# Default timeout is 1 minute
timeout="${TIMEOUT:-60}"

# Gets a health check of a container
# Usage example:
# health_status <container_name>
function health_status
{
    docker inspect --format '{{json .State.Health.Status }}' "$1"
}

for i in $(seq 0 "${timeout}"); do
    if [ $(health_status "$container")"" = "\"healthy\"" ]; then
        echo -e "\nWaiting is done after $i seconds"
        exit 0
    fi
    echo -n "."
    sleep 1
done
echo -e "\nKilled by timeout"
exit 1
