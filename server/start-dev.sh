#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.

mnesia=$(ls | grep Mnesia)
if [ $mnesia ]; then arg="start"; else arg="init"; fi

if [ ! -d "priv/tmp" ]; then mkdir -p "priv/tmp"; fi
if [ ! -d "priv/repository/binary" ]; then mkdir -p "priv/repository/binary"; fi
if [ ! -d "priv/repository/source" ]; then mkdir -p "priv/repository/source"; fi

exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname sup_mochiweb_dev \
    -s sup_mochiweb \
    -s reloader \
    -s lager start \
    -s sup_db $arg
# -s Module Func starts given Func in Module; Func can be ommited
