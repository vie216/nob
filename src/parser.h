#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  ExprKindBinOp = 0,
  ExprKindLit,
  ExprKindBlock,
  ExprKindIdent,
  ExprKindCall,
  ExprKindVar,
  ExprKindFunc,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprLit   ExprLit;
typedef struct ExprBlock ExprBlock;
typedef struct ExprIdent ExprIdent;
typedef struct ExprCall  ExprCall;
typedef struct ExprVar   ExprVar;
typedef struct ExprFunc  ExprFunc;

typedef union {
  ExprBinOp *bin_op;
  ExprLit   *lit;
  ExprBlock *block;
  ExprIdent *ident;
  ExprCall  *call;
  ExprVar   *var;
  ExprFunc  *func;
} ExprAs;

typedef struct {
  ExprKind kind;
  ExprAs   as;
} Expr;

struct ExprBinOp {
  Str  op;
  Expr lhs;
  Expr rhs;
};

typedef enum {
  LitKindInt = 0,
  LitKindStr,
} LitKind;

struct ExprLit {
  LitKind kind;
  Str     lit;
};

struct ExprBlock {
  Expr *items;
  i32   len, cap;
};

struct ExprIdent {
  Str ident;
};

struct ExprCall {
  Expr       func;
  ExprBlock *args;
};

struct ExprVar {
  Str  name;
  Expr value;
};

typedef struct {
  Str *items;
  i32  len, cap;
} Args;

struct ExprFunc {
  Str  name;
  Args args;
  Expr body;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
