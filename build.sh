CFLAGS="-Wall -Wextra"
SRC="$(find src -name "*.c")"
LIBS=""

cc -o nob $CFLAGS "${@:1}" $SRC $LIBS
