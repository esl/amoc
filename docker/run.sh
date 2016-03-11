#!/bin/bash

set -x

sed -e "s/AMOC_GRAPHITE_HOST/${AMOC_GRAPHITE_HOST}/" /sys.config.template > \
    /home/amoc/amoc/releases/0.9/sys.config
sed -i -e "s/AMOC_PREFIX/${AMOC_PREFIX}/" /home/amoc/amoc/releases/0.9/sys.config

/sbin/my_init
