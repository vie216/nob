#ifndef STR_H
#define STR_H

#include <stdbool.h>

#include "defs.h"

#define STR(ptr, len) ((Str) { ptr, len })

typedef struct {
  char *ptr;
  i32   len;
} Str;

Str str_new(char *str);
bool str_eq(Str a, Str b);
void str_print(Str str);
void str_println(Str str);

typedef struct {
  char *buffer;
  i32   cap;
  i32   len;
} StringBuilder;

void sb_push_str(StringBuilder *sb, Str str);
void sb_push(StringBuilder *sb, char *str);

#endif // STR_H
