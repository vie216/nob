#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  ExprKindLit = 0,
  ExprKindBlock,
  ExprKindIdent,
  ExprKindCall,
  ExprKindVar,
  ExprKindFunc,
  ExprKindIf,
  ExprKindWhile,
} ExprKind;

typedef struct ExprLit   ExprLit;
typedef struct ExprBlock ExprBlock;
typedef struct ExprIdent ExprIdent;
typedef struct ExprCall  ExprCall;
typedef struct ExprVar   ExprVar;
typedef struct ExprFunc  ExprFunc;
typedef struct ExprIf    ExprIf;
typedef struct ExprWhile ExprWhile;

typedef struct Def Def;

typedef union {
  ExprLit   *lit;
  ExprBlock *block;
  ExprIdent *ident;
  ExprCall  *call;
  ExprVar   *var;
  ExprFunc  *func;
  ExprIf    *eef;
  ExprWhile *whail;
} ExprAs;

typedef struct {
  ExprKind kind;
  ExprAs   as;
} Expr;

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
  Def *def;
};

struct ExprCall {
  Expr       func;
  ExprBlock *args;
};

struct ExprVar {
  Str  name;
  Expr value;
  Def *def;
};

typedef struct {
  Str *items;
  i32  len, cap;
} Args;

struct ExprFunc {
  Str  name;
  Args args;
  Expr body;
  Def *def;
};

struct ExprIf {
  Expr cond;
  Expr body;
  Expr elze;
  bool has_else;
};

struct ExprWhile {
  Expr cond;
  Expr body;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
