#ifndef TYPE_H
#define TYPE_H

#include "parser.h"

typedef enum {
  TypeKindUnit = 0,
  TypeKindInt,
  TypeKindPtr,
  TypeKindFunc,
  TypeKindError,
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
} Type;

typedef enum {
  IntKindS64 = 0,
  IntKindU8,
} IntKind;

struct TypeInt {
  IntKind kind;
};

struct TypePtr {
  Type points_to;
  bool is_str_lit;
};

struct TypeFunc {
  Type  result_type;
  Def  *arg_defs;
  i32   arity;
};

struct Def {
  Str  name, loc;
  Type type;
  bool is_intrinsic;
  Def *next;
};

typedef struct {
  ExprFunc *expr;
  Def      *arg_defs;
  i32       arity;
  i32       scope_size;
} Func;

typedef struct {
  Func *items;
  i32   len, cap;
} Funcs;

typedef struct {
  Def   *defs;
  Funcs  funcs;
} Metadata;

Metadata type_check(Expr program, Def *intrinsic_defs);

#endif // TYPE_H
