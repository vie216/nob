#ifndef GEN_H
#define GEN_H

#include "type.h"

typedef struct {
  Str *items;
  i32 len, cap;
} Strings;

typedef struct {
  Func *func;
  i32   stack_pointer;
  i32   max_regs_used;
  i32   max_arg_regs_used;
} FuncCtx;

typedef struct {
  StringBuilder sb;
  Strings       strings;
  FuncCtx       ctx;
  i32           regs_used;
  i32           label_count;
} Generator;

Str gen_linux_x86_64(Metadata meta);

#endif // GEN_H
