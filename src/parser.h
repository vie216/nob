#ifndef PARSER_H
#define PARSER_H

#include "str.h"

typedef enum {
  TypeExprKindIdent = 0,
  TypeExprKindPtr,
} TypeExprKind;

typedef struct TypeExprIdent TypeExprIdent;
typedef struct TypeExprPtr   TypeExprPtr;

typedef union {
  TypeExprIdent *ident;
  TypeExprPtr   *ptr;
} TypeExprAs;

typedef struct {
  TypeExprKind kind;
  TypeExprAs   as;
} TypeExpr;

struct TypeExprIdent {
  Str ident;
};

struct TypeExprPtr {
  TypeExpr points_to;
};

typedef enum {
  ExprKindLit = 0,
  ExprKindBlock,
  ExprKindIdent,
  ExprKindCall,
  ExprKindVar,
  ExprKindFunc,
  ExprKindIf,
  ExprKindWhile,
  ExprKindRet,
} ExprKind;

typedef struct ExprLit   ExprLit;
typedef struct ExprBlock ExprBlock;
typedef struct ExprIdent ExprIdent;
typedef struct ExprCall  ExprCall;
typedef struct ExprVar   ExprVar;
typedef struct ExprFunc  ExprFunc;
typedef struct ExprIf    ExprIf;
typedef struct ExprWhile ExprWhile;
typedef struct ExprRet   ExprRet;

typedef union {
  ExprLit   *lit;
  ExprBlock *block;
  ExprIdent *ident;
  ExprCall  *call;
  ExprVar   *var;
  ExprFunc  *func;
  ExprIf    *eef;
  ExprWhile *whail;
  ExprRet   *ret;
} ExprAs;

typedef struct {
  ExprKind kind;
  ExprAs   as;
} Expr;

typedef struct Def Def;

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
  Str       name;
  Expr      value;
  bool      has_type;
  TypeExpr  type;
  Def      *def;
};

typedef struct {
  Str      name;
  TypeExpr type;
} Arg;

typedef struct {
  Arg *items;
  i32  len, cap;
} Args;

struct ExprFunc {
  Str       name;
  Args      args;
  Expr      body;
  TypeExpr  result_type;
  Def      *def;
  i32       func_index;
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

struct ExprRet {
  Expr result;
  bool has_result;
};

Expr parse_program(Str source, char *file_path);

#endif // PARSER_H
