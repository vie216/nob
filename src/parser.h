#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  ExprKindBinOp = 0,
  ExprKindIntLit,
  ExprKindBlock,
  ExprKindIdent,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprIntLit ExprIntLit;
typedef struct ExprBlock ExprBlock;
typedef struct ExprIdent ExprIdent;

typedef union {
  ExprBinOp  *bin_op;
  ExprIntLit *int_lit;
  ExprBlock  *block;
  ExprIdent  *ident;
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
  Expr     *items;
  i32       len, cap;
};

struct ExprIdent {
  ExprKind  kind;
  Str       ident;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
