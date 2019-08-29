#!/bin/bash

set -x

AMOC_GRAPHITE_HOST=${AMOC_GRAPHITE_HOST:-127.0.0.1}
AMOC_GRAPHITE_PORT=${AMOC_GRAPHITE_PORT:-2003}
AMOC_EXTRA_CODE_PATHS=${AMOC_EXTRA_CODE_PATHS:-""}

SYSCONFIG="/home/amoc/amoc/releases/0.9.0/sys.config"
VMARGS="/home/amoc/amoc/releases/0.9.0/vm.args"
sed -e "s/AMOC_GRAPHITE_HOST/${AMOC_GRAPHITE_HOST}/" /sys.config.template > \
    ${SYSCONFIG}
sed -i -e "s/AMOC_GRAPHITE_PORT/${AMOC_GRAPHITE_PORT}/" ${SYSCONFIG}
sed -i -e "s/AMOC_PREFIX/${AMOC_PREFIX}/" ${SYSCONFIG}
sed -i -e "s/AMOC_HOSTS/${AMOC_HOSTS}/" ${SYSCONFIG}

if [[ -n "${AMOC_EXTRA_CODE_PATHS// /}" ]]; then
    #if the var is not empty and contains sth else then just spaces
    echo "-pz ${aMOC_EXTRA_CODE_PATHS}" >> ${VMARGS}
fi

/sbin/my_init
