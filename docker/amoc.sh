#!/bin/bash
exec /sbin/setuser amoc /home/amoc/amoc/bin/amoc console -noshell -noinput +Bd \
    -kernel inet_dist_listen_min 5000 inet_dist_listen_max 5030
