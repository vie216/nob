#include "type.h"
#include "log.h"
#include "defs.h"
#include "arena.h"

typedef struct {
  Def   *defs;
  Def   *intrinsic_defs;
  Funcs  funcs;
  bool   has_error;
  bool   found_main;
} Checker;

static bool type_eq(Type a, Type b) {
  if (a.kind == TypeKindError ||
      b.kind == TypeKindError)
    return true;

  if (a.kind != b.kind)
    return false;

  switch (a.kind) {
  case TypeKindUnit: {
    return true;
  }

  case TypeKindInt: {
    return a.as.eent->kind == b.as.eent->kind;
  }

  case TypeKindPtr: {
    bool a_is_str_lit = a.as.ptr->is_str_lit;
    bool b_is_str_lit = b.as.ptr->is_str_lit;

    return type_eq(a.as.ptr->points_to, b.as.ptr->points_to) &&
           (a_is_str_lit || a_is_str_lit == b_is_str_lit);
  }

  case TypeKindFunc: {
    if (!type_eq(a.as.func->result_type, b.as.func->result_type) ||
        a.as.func->arity != b.as.func->arity)
      return false;

    Def *a_arg = a.as.func->arg_defs;
    Def *b_arg = b.as.func->arg_defs;
    for (i32 i = 0; i < a.as.func->arity; ++i) {
      if (!type_eq(a_arg->type, b_arg->type))
        return false;

      a_arg = a_arg->next;
      b_arg = b_arg->next;
    }

    return true;
  }

  case TypeKindError: break;
  }

  ERROR("Unreachable\n");
  exit(1);
}

static Type type_from_type_expr(TypeExpr expr) {
  switch (expr.kind) {
  case TypeExprKindIdent: {
    if (str_eq(expr.as.ident->ident, STR_LIT("unit"))) {
      return (Type) { TypeKindUnit };
    } else if (str_eq(expr.as.ident->ident, STR_LIT("s64"))) {
      TypeInt *eent = aalloc(sizeof(TypeInt));
      eent->kind = IntKindS64;
      return (Type) { TypeKindInt, { .eent = eent } };
    } else if (str_eq(expr.as.ident->ident, STR_LIT("u8"))) {
      TypeInt *eent = aalloc(sizeof(TypeInt));
      eent->kind = IntKindU8;
      return (Type) { TypeKindInt, { .eent = eent } };
    }

    ERROR("There is no support for custom types yet\n");
    exit(1);
  }

  case TypeExprKindPtr: {
    TypePtr *ptr = aalloc(sizeof(TypePtr));
    ptr->points_to = type_from_type_expr(expr.as.ptr->points_to);
    return (Type) { TypeKindPtr, { .ptr = ptr } };
  }
  }

  ERROR("Unreachable\n");
  exit(1);
}

static i32 expr_scope_size(Expr expr) {
  i32 scope_size = 0;

  switch (expr.kind) {
  case ExprKindLit: break;

  case ExprKindBlock: {
    for (i32 i = 0; i < expr.as.block->len; ++i)
      scope_size += expr_scope_size(expr.as.block->items[i]);
  } break;

  case ExprKindIdent: break;

  case ExprKindCall: {
    scope_size = expr_scope_size(expr.as.call->func);
    for (i32 i = 0; i < expr.as.call->args->len; ++i)
      scope_size += expr_scope_size(expr.as.call->args->items[i]);
  } break;

  case ExprKindVar: {
    scope_size = 8 + expr_scope_size(expr.as.var->value);
  } break;

  case ExprKindFunc: break;

  case ExprKindIf: {
    expr_scope_size(expr.as.eef->cond);
    expr_scope_size(expr.as.eef->body);
    if (expr.as.eef->has_else)
      expr_scope_size(expr.as.eef->elze);
  } break;

  case ExprKindWhile: {
    expr_scope_size(expr.as.whail->cond);
    expr_scope_size(expr.as.whail->body);
  } break;
  }

  return scope_size;
}

static Def *checker_lookup_def(Checker *checker, Str def_name) {
  Def *def = checker->defs;

  while (def) {
    if (str_eq(def->name, def_name))
      return def;
    def = def->next;
  }

  for (i32 i = 0; i < checker->funcs.len; ++i) {
    def = checker->funcs.items[i].expr->def;
    if (str_eq(def->name, def_name))
      return def;
  }

  def = checker->intrinsic_defs;

  while (def) {
    if (str_eq(def->name, def_name))
      return def;
    def = def->next;
  }

  return NULL;
}

static Def *checker_lookup_def_typed(Checker *checker, Str def_name, Type def_type) {
  Def *def = checker->defs;

  while (def) {
    if (str_eq(def->name, def_name) &&
        type_eq(def->type, def_type))
      return def;
    def = def->next;
  }

  for (i32 i = 0; i < checker->funcs.len; ++i) {
    def = checker->funcs.items[i].expr->def;
    if (str_eq(def->name, def_name) && type_eq(def->type, def_type))
      return def;
  }

  def = checker->intrinsic_defs;

  while (def) {
    if (str_eq(def->name, def_name))
      return def;
    def = def->next;
  }

  return NULL;
}

static void checker_collect_funcs(Checker *checker, Expr expr) {
  switch (expr.kind) {
  case ExprKindLit: break;

  case ExprKindBlock: break;

  case ExprKindIdent: break;

  case ExprKindCall: {
    checker_collect_funcs(checker, expr.as.call->func);
    for (i32 i = 0; i < expr.as.call->args->len; ++i)
      checker_collect_funcs(checker, expr.as.call->args->items[i]);
  } break;

  case ExprKindVar: {
    checker_collect_funcs(checker, expr.as.var->value);
  } break;

  case ExprKindFunc: {
    Args args = expr.as.func->args;

    TypeFunc *func = aalloc(sizeof(TypeFunc));
    func->result_type = type_from_type_expr(expr.as.func->result_type);
    func->arg_defs = NULL;
    func->arity = args.len;
    Type type = { TypeKindFunc, { .func = func } };

    Def *arg_defs_end = NULL;
    for (i32 i = 0; i < args.len; ++i) {
      LL_PREPEND(func->arg_defs, arg_defs_end, Def);
      arg_defs_end->name = args.items[i].name;
      arg_defs_end->type = type_from_type_expr(args.items[i].type);
    }

    if (checker_lookup_def_typed(checker, expr.as.func->name, type)) {
      ERROR("Function `"STR_FMT"` redefined\n",
            STR_ARG(expr.as.func->name));
      checker->has_error = true;
      return;
    }

    expr.as.func->def = aalloc(sizeof(Def));
    expr.as.func->def->name = expr.as.func->name;
    expr.as.func->def->type = type;
    expr.as.func->def->is_intrinsic = false;

    Func new_func = {
      .expr = expr.as.func,
      .arg_defs = func->arg_defs,
      .arity = func->arity,
    };
    DA_APPEND(checker->funcs, new_func);
  } break;

  case ExprKindIf: {
    checker_collect_funcs(checker, expr.as.eef->cond);
    checker_collect_funcs(checker, expr.as.eef->body);
    if (expr.as.eef->has_else)
      checker_collect_funcs(checker, expr.as.eef->elze);
  } break;

  case ExprKindWhile: {
    checker_collect_funcs(checker, expr.as.whail->cond);
    checker_collect_funcs(checker, expr.as.whail->body);
  } break;
  }
}

static Type checker_type_check_expr(Checker *checker, Expr expr) {
  switch (expr.kind) {
  case ExprKindBlock: {
    Def *prev_defs = checker->defs;

    for (i32 i = 0; i < expr.as.block->len; ++i)
      checker_collect_funcs(checker, expr.as.block->items[i]);
    for (i32 i = 0; i + 1 < expr.as.block->len; ++i)
      checker_type_check_expr(checker, expr.as.block->items[i]);

    Type result;
    if (expr.as.block->len == 0) {
      result = (Type) { TypeKindUnit };
    } else {
      Expr last_expr = expr.as.block->items[expr.as.block->len - 1];
      result = checker_type_check_expr(checker, last_expr);
    }

    checker->defs = prev_defs;

    return result;
  }

  case ExprKindLit: {
    if (expr.as.lit->kind == LitKindInt) {
      TypeInt *eent = aalloc(sizeof(TypeInt));
      eent->kind = IntKindS64;
      return (Type) { TypeKindInt, { .eent = eent } };
    } else if (expr.as.lit->kind == LitKindStr) {
      TypePtr *ptr = aalloc(sizeof(TypePtr));
      ptr->points_to.kind = TypeKindInt;
      ptr->points_to.as.eent = aalloc(sizeof(TypeInt));
      ptr->points_to.as.eent->kind = IntKindU8;
      ptr->is_str_lit = true;
      return (Type) { TypeKindPtr, { .ptr = ptr } };
    }

    ERROR("Unreachable\n");
    exit(1);
  }

  case ExprKindIdent: {
    expr.as.ident->def = checker_lookup_def(checker, expr.as.ident->ident);
    if (!expr.as.ident->def) {
      ERROR("Undeclared identifier: `"STR_FMT"`\n",
            STR_ARG(expr.as.ident->ident));
      checker->has_error = true;
      return (Type) { TypeKindError, {0} };
    }

    return expr.as.ident->def->type;
  } break;

  case ExprKindVar: {
    Type value_type = checker_type_check_expr(checker, expr.as.var->value);

    if (expr.as.var->has_type) {
      Type var_type = type_from_type_expr(expr.as.var->type);
      if (!type_eq(value_type, var_type)) {
        ERROR("Type of variable and type of value assigned to it are different\n");
        INFO("TODO: type printing\n");
        checker->has_error = true;
      }
    }

    LL_APPEND(checker->defs, Def);
    checker->defs->name = expr.as.var->name;
    checker->defs->type = value_type;
    expr.as.var->def = checker->defs;

    return checker->defs->type;
  } break;

  case ExprKindCall: {
    Type func_type = checker_type_check_expr(checker, expr.as.call->func);
    if (func_type.kind != TypeKindFunc) {
      ERROR("Only functions can be called\n");
      INFO("TODO: type printing\n");
      checker->has_error = true;
      return func_type;
    }

    ExprBlock *args = expr.as.call->args;
    if (func_type.as.func->arity != args->len) {
      ERROR("Expected %d arguments, but got %d\n",
            func_type.as.func->arity, args->len);
      checker->has_error = true;
      return func_type;
    }

    Def *arg = func_type.as.func->arg_defs;
    for (i32 i = 0; i < args->len; ++i) {
      Type arg_type = checker_type_check_expr(checker, args->items[i]);
      if (!type_eq(arg_type, arg->type)) {
        ERROR("Unexpected argument type\n");
        INFO("TODO: type printing\n");
        checker->has_error = true;
      }

      arg = arg->next;
    }

    return func_type.as.func->result_type;
  } break;

  case ExprKindFunc: {
    if (str_eq(expr.as.func->name, STR("main", 4)))
      checker->found_main = true;

    Def *prev_defs = checker->defs;
    checker->defs = NULL;

    Type func_type = expr.as.func->def->type;
    Type result_type = func_type.as.func->result_type;

    Type body_type = checker_type_check_expr(checker, expr.as.func->body);
    if (result_type.kind != TypeKindUnit &&
        !type_eq(body_type, result_type)) {
      ERROR("Unexpected function return type\n");
      INFO("TODO: type printing\n");
      checker->has_error = true;
    }

    checker->defs = prev_defs;

    return func_type;
  } break;

  case ExprKindIf: {
    Type cond_type = checker_type_check_expr(checker, expr.as.eef->cond);
    if (cond_type.kind != TypeKindInt && cond_type.kind != TypeKindPtr) {
      ERROR("Expected integer or pointer\n");
      INFO("TODO: type printing\n");
      checker->has_error = true;
    }

    Type body_type = checker_type_check_expr(checker, expr.as.eef->body);

    if (expr.as.eef->has_else) {
      Type else_type = checker_type_check_expr(checker, expr.as.eef->elze);
      if (!type_eq(else_type, body_type)) {
        ERROR("If and else should return the same type\n");
        checker->has_error = true;
      }

      return else_type;
    }

    return (Type) { TypeKindUnit };
  } break;

  case ExprKindWhile: {
    Type cond_type = checker_type_check_expr(checker, expr.as.whail->cond);
    if (cond_type.kind != TypeKindInt && cond_type.kind != TypeKindPtr) {
      ERROR("Expected integer or pointer\n");
      INFO("TODO: type printing\n");
      checker->has_error = true;
    }

    checker_type_check_expr(checker, expr.as.whail->body);

    return (Type) { TypeKindUnit };
  } break;
  }

  ERROR("Unreachable\n");
  exit(1);
}

Metadata type_check(Expr program, Def *intrinsic_defs) {
  Checker checker = {
    .intrinsic_defs = intrinsic_defs,
  };

  checker_type_check_expr(&checker, program);

  if (!checker.found_main) {
    ERROR("Function `main` is required\n");
    checker.has_error = true;
  }

  if (checker.has_error)
    exit(1);

  for (i32 i = 0; i < checker.funcs.len; ++i) {
    Func *func = checker.funcs.items + i;
    func->scope_size = expr_scope_size(func->expr->body);
  }

  return (Metadata) {
    .funcs = checker.funcs,
    .defs = checker.defs,
  };
}
