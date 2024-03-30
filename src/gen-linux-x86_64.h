#ifndef GEN_H
#define GEN_H

#include "checker.h"

typedef struct {
  Str *items;
  i32 len, cap;
} Strings;

typedef struct {
  i32 scope_size;
  i32 stack_pointer;
  i32 max_regs_used;
  i32 max_arg_regs_used;
} FunctionContext;

typedef struct {
  FunctionContext ctx;
  StringBuilder   sb;
  Strings         strings;
  i32             regs_used;
  i32             ifs_count;
} Generator;

Str gen_linux_x86_64(Metadata meta);
Def *intrinsic_defs_linux_x86_64(void);

#endif // GEN_H
