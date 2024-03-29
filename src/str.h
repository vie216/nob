#ifndef STR_H
#define STR_H

#include <stdio.h>

#include "defs.h"

#define STR(ptr, len) ((Str) { ptr, len })
#define STR_LIT(ptr) ((Str) { ptr, sizeof(ptr) - 1 })

typedef struct {
  char *ptr;
  i32   len;
} Str;

Str str_new(char *str);
bool str_eq(Str a, Str b);
void str_fprint(FILE *stream, Str str);
void str_fprintln(FILE *stream, Str str);
void str_print(Str str);
void str_println(Str str);

typedef struct {
  char *buffer;
  i32   cap;
  i32   len;
} StringBuilder;

Str sb_to_str(StringBuilder sb);
void sb_push(StringBuilder *sb, char *str);
void sb_push_str(StringBuilder *sb, Str str);
void sb_push_i32(StringBuilder *sb, i32 num);

#endif // STR_H
