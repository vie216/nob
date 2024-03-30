#ifndef CHECKER_H
#define CHECKER_H

#include "parser.h"

struct Def {
  Str  name, loc;
  i32  size;
  bool is_intrinsic;
  Def *next;
};

typedef struct {
  ExprFunc *expr;
  i32       scope_size;
  Def      *arg_defs;
  i32       args_count;
} Func;

typedef struct {
  Func *items;
  i32   len, cap;
} Funcs;

typedef struct {
  Def   *defs;
  Funcs  funcs;
} Metadata;

Metadata add_metadata(Expr program, Def *intrinsic_defs);

#endif // CHECKER_H
