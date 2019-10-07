#!/bin/bash
EXEC_PATH=${EXEC_PATH:-/home/amoc/amoc/bin/amoc}
exec /sbin/setuser amoc $EXEC_PATH console -noshell -noinput +Bd
