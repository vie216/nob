#ifndef CHECKER_H
#define CHECKER_H

#include "parser.h"

typedef struct {
  ExprFunc **items;
  i32        len, cap;
} Functions;

Functions add_metadata(Expr *program);

#endif // CHECKER_H
