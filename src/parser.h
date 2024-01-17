#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  ExprKindBinOp = 0,
  ExprKindIntLit,
  ExprKindBlock,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprIntLit ExprIntLit;
typedef struct ExprBlock ExprBlock;

typedef union {
  ExprBinOp  *bin_op;
  ExprIntLit *int_lit;
  ExprBlock  *block;
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

struct ExprBlock {
  ExprKind  kind;
  Expr     *exprs;
  i32       len;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
