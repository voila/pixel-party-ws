#!/bin/sh
erl -pa ebin deps/*/ebin -s pixel_party \
    -eval "io:format(\"Point your browser at http://localhost:8080/~n\")."

