#ifndef TYPE_H
#define TYPE_H

#include "parser.h"

typedef enum {
  TypeKindUnit = 0,
  TypeKindInt,
  TypeKindPtr,
  TypeKindFunc,
} TypeKind;

typedef struct TypeInt  TypeInt;
typedef struct TypePtr  TypePtr;
typedef struct TypeFunc TypeFunc;

typedef union {
  TypeInt  *eent;
  TypePtr  *ptr;
  TypeFunc *func;
} TypeAs;

typedef struct {
  TypeKind kind;
  TypeAs   as;
  Str      size;
} Type;

struct TypeInt {
  bool signedd;
};

struct TypePtr {
  Type points_to;
};

struct TypeFunc {
  Str   name;
  Type  result_type;
  Def  *arg_defs;
  i32   arity;
};

struct Def {
  Str   name, loc;
  Type  type;
  bool  is_intrinsic;
  Def  *next;
};

typedef struct {
  ExprFunc *expr;
  Def      *arg_defs;
  i32       arity;
} Func;

typedef struct {
  Func *items;
  i32   len, cap;
} Funcs;

typedef struct {
  Def     *defs;
  Funcs    funcs;
} Metadata;

Metadata type_check(Expr program, Def *intrinsic_defs);

#endif // TYPE_H
