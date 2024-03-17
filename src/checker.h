#ifndef CHECKER_H
#define CHECKER_H

#include "parser.h"

struct Def {
  Str  name, loc;
  i32  size;
  Def *next;
};

typedef struct {
  ExprFunc *expr;
  i32       scope_size;
} Func;

typedef struct {
  Func *items;
  i32   len, cap;
} Funcs;

typedef struct {
  Def   *def;
  Funcs  funcs;
} Metadata;

Metadata add_metadata(Expr program);

#endif // CHECKER_H
