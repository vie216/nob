#ifndef GEN_H
#define GEN_H

#include "type.h"

typedef struct {
  Str name;
  i32 offset;
} Var;

typedef struct {
  Var *items;
  i32  len, cap;
} Vars;

typedef struct {
  Str *items;
  i32  len, cap;
} Strings;

typedef struct {
  Func *func;
  i32   stack_pointer;
  i32   max_regs_used;
  i32   max_arg_regs_used;
  i32   max_stack_used;
} FuncCtx;

typedef struct {
  StringBuilder sb;
  Vars          vars;
  Strings       strings;
  FuncCtx       ctx;
  i32           regs_used;
  i32           stack_used;
  i32           label_count;
} Generator;

Str gen_linux_x86_64(Metadata meta);

#endif // GEN_H
