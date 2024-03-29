#ifndef GEN_H
#define GEN_H

#include "checker.h"

typedef struct {
  Str *items;
  i32 len, cap;
} Strings;

typedef struct {
  StringBuilder sb;
  Strings       strings;
  i32           regs_used;
  i32           scope_size;
  i32           stack_pointer;
  i32           ifs_count;
} Generator;

char *gen_linux_x86_64(Metadata meta);
Def *intrinsic_defs_linux_x86_64(void);

#endif // GEN_H
