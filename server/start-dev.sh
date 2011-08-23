#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname mochisup_dev \
    -s mochisup \
    -s reloader \
    -s db start \
    -s sup_server_connection start
# -s Module Func starts given Func in Module; Func can be ommited
