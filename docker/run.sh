#!/bin/bash

set -x

export AMOC_GRAPHITE_HOST="${AMOC_GRAPHITE_HOST:+\"${AMOC_GRAPHITE_HOST}\"}"
export AMOC_GRAPHITE_PREFIX="${AMOC_PREFIX:+\"${AMOC_PREFIX}\"}"

export AMOC_HOSTS="[${AMOC_HOSTS}]"

/sbin/my_init
