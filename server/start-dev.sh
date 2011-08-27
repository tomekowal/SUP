#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname sup_mochiweb_dev \
    -s sup_mochiweb \
    -s reloader \
    -s sup_db start
# -s Module Func starts given Func in Module; Func can be ommited
