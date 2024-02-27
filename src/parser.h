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
  ExprKindIf,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprLit   ExprLit;
typedef struct ExprBlock ExprBlock;
typedef struct ExprIdent ExprIdent;
typedef struct ExprCall  ExprCall;
typedef struct ExprVar   ExprVar;
typedef struct ExprFunc  ExprFunc;
typedef struct ExprIf    ExprIf;

typedef union {
  ExprBinOp *bin_op;
  ExprLit   *lit;
  ExprBlock *block;
  ExprIdent *ident;
  ExprCall  *call;
  ExprVar   *var;
  ExprFunc  *func;
  ExprIf    *eef;
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
  // Metadata
  Str  target_name;
  Expr target_expr;
};

struct ExprCall {
  Expr       func;
  ExprBlock *args;
};

struct ExprVar {
  Str  name;
  Expr value;
  // Metadata
  Str loc;
  i32 size;
};

typedef struct {
  Str *items;
  i32  len, cap;
} Args;

struct ExprFunc {
  Str  name;
  Args args;
  Expr body;
  // Metadata
  Str loc;
  i32 scope_size;
};

struct ExprIf {
  Expr cond;
  Expr body;
  Expr elze;
  bool has_else;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
