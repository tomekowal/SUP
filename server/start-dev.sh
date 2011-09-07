#!/bin/sh
mnesia=$(ls | grep Mnesia)
if [ $mnesia ]; then arg="start"; else arg="init"; fi

exec erl -pa $PWD/ebin \
     -pa deps/*/ebin \
     -sname sup \
     -boss developing_app server \
     -boot start_sasl -config boss -s reloader -s boss \
     -s sup_db $arg
