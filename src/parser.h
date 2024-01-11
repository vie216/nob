#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  ExprKindBinOp = 0,
  ExprKindIntLit,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprIntLit ExprIntLit;

typedef union {
  ExprBinOp  *bin_op;
  ExprIntLit *int_lit;
} Expr;

struct ExprBinOp {
  ExprKind kind;
  Str      op;
  Expr     lhs;
  Expr     rhs;
};

struct ExprIntLit {
  ExprKind  kind;
  Str       lit;
};

Expr parse_program(Str source);

#endif // PARSER_H
