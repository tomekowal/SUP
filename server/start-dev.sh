#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.

mnesia=$(ls | grep Mnesia)
if [ $mnesia ]; then arg="start"; else arg="init"; fi

exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname sup_mochiweb_dev \
    -s sup_mochiweb \
    -s reloader \
    -s lager start \
    -s sup_db $arg
# -s Module Func starts given Func in Module; Func can be ommited
