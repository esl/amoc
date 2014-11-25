#!/bin/sh

if [ "$#" -ne 3 ]; then
    echo "illegal number of parameters"
    echo 'specify "scenario name" "From" "To"'
    exit 1
fi

./rebar compile && erl -config priv/app -env ERL_FULLSWEEP_AFTER 2 -pa ./deps/*/ebin ./ebin -s amoc do $1 $2 $3
