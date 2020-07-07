#!/bin/bash
exec_path="${EXEC_PATH:-/home/amoc/amoc/bin/amoc}"
exec /sbin/setuser amoc "$exec_path" console -noshell -noinput +Bd
