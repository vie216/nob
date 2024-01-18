#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  ExprKindBinOp = 0,
  ExprKindIntLit,
  ExprKindStrLit,
  ExprKindBlock,
  ExprKindIdent,
  ExprKindCall,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprIntLit ExprIntLit;
typedef struct ExprStrLit ExprStrLit;
typedef struct ExprBlock ExprBlock;
typedef struct ExprIdent ExprIdent;
typedef struct ExprCall ExprCall;

typedef union {
  ExprBinOp  *bin_op;
  ExprIntLit *int_lit;
  ExprStrLit *str_lit;
  ExprBlock  *block;
  ExprIdent  *ident;
  ExprCall  *call;
} ExprAs;

typedef struct {
  ExprKind kind;
  ExprAs   as;
} Expr;

struct ExprBinOp {
  Str      op;
  Expr     lhs;
  Expr     rhs;
};

struct ExprIntLit {
  Str       lit;
};

struct ExprStrLit {
  Str       lit;
};

struct ExprBlock {
  Expr     *items;
  i32       len, cap;
};

struct ExprIdent {
  Str       ident;
};

struct ExprCall {
  Str       name;
  ExprBlock args;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
