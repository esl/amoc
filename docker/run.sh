#!/bin/bash

set -x

AMOC_GRAPHITE_HOST=${AMOC_GRAPHITE_HOST:-127.0.0.1}
AMOC_GRAPHITE_PORT=${AMOC_GRAPHITE_PORT:-2003}

SYSCONFIG="/home/amoc/amoc/releases/0.9.0/sys.config"
sed -e "s/AMOC_GRAPHITE_HOST/${AMOC_GRAPHITE_HOST}/" /sys.config.template > \
    ${SYSCONFIG}
sed -i -e "s/AMOC_GRAPHITE_PORT/${AMOC_GRAPHITE_PORT}/" ${SYSCONFIG}
sed -i -e "s/AMOC_PREFIX/${AMOC_PREFIX}/" ${SYSCONFIG}
sed -i -e "s/AMOC_HOSTS/${AMOC_HOSTS}/" ${SYSCONFIG}

/sbin/my_init
