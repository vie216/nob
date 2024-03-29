#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "str.h"

Str str_new(char *str) {
  return (Str) { .ptr = str, .len = strlen(str) };
}

bool str_eq(Str a, Str b) {
  if (a.len != b.len)
    return false;

  for (i32 i = 0; i < a.len; ++i)
    if (a.ptr[i] != b.ptr[i])
      return false;

  return true;
}

void str_fprint(FILE *stream, Str str) {
  for (i32 i = 0; i < str.len; ++i)
    putc(str.ptr[i], stream);
}

void str_fprintln(FILE *stream, Str str) {
  str_fprint(stream, str);
  putc('\n', stream);
}

void str_print(Str str) {
  str_fprint(stdout, str);
}

void str_println(Str str) {
  str_fprintln(stdout, str);
}

static void sb_reserve_space(StringBuilder *sb, i32 amount) {
  if (amount > sb->cap - sb->len) {
    if (sb->cap != 0) {
      while (amount > sb->cap - sb->len)
        sb->cap *= 2;

      sb->buffer = realloc(sb->buffer, sb->cap + 1);
    } else {
      sb->cap += amount;
      sb->buffer = malloc(sb->cap + 1);
    }
  }
}

Str sb_to_str(StringBuilder sb) {
  return (Str) {
    .ptr = sb.buffer,
    .len = sb.len,
  };
}

void sb_push(StringBuilder *sb, char *str) {
  sb_push_str(sb, str_new(str));
}

void sb_push_str(StringBuilder *sb, Str str) {
  sb_reserve_space(sb, str.len);
  memmove(sb->buffer + sb->len, str.ptr, str.len);
  sb->len += str.len;
}

void sb_push_i32(StringBuilder *sb, i32 num) {
  i32 _num = num;
  i32 len = 1;

  while (_num >= 10) {
    _num /= 10;
    len++;
  }

  snprintf(sb->buffer + sb->len, len + 1, "%d", num);
  sb->len += len;
}
