#!/usr/bin/sh

CFLAGS="-Wall -Wextra"
SRC="$(find src -name "*.c")"
LIBS="-lm"

cc -o nob $CFLAGS "${@:1}" $SRC $LIBS
