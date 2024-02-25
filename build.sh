#!/usr/bin/sh

CFLAGS="-Wall -Wextra"
SRC="$(find src -name "*.c")"

cc -o nob $CFLAGS "${@:1}" $SRC
