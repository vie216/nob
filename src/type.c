#include "type.h"
#include "log.h"
#include "defs.h"
#include "arena.h"

typedef struct {
  Def   *defs;
  Funcs funcs;
  Type  current_func_result_type;
  bool  inside_of_func;
  bool  has_error;
  bool  found_main;
} Checker;

static bool type_eq(Type a, Type b) {
  if (a.kind != b.kind)
    return false;

  switch (a.kind) {
  case TypeKindUnit: {
    return true;
  }

  case TypeKindInt: {
    return a.as.eent->signedd == b.as.eent->signedd &&
           str_eq(a.size, b.size);
  }

  case TypeKindPtr: {
    return type_eq(a.as.ptr->points_to, b.as.ptr->points_to);
  }

  case TypeKindFunc: {
    if (!str_eq(a.as.func->name, b.as.func->name) ||
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
  }

  ERROR("Unreachable\n");
  exit(1);
}

static Type type_from_type_expr(TypeExpr expr) {
  switch (expr.kind) {
  case TypeExprKindUnit: {
    return (Type) {
      .kind = TypeKindUnit,
      .size = STR_LIT("0"),
    };
  }

  case TypeExprKindIdent: {
    if (str_eq(expr.as.ident->ident, STR_LIT("s64"))) {
      TypeInt *eent = aalloc(sizeof(TypeInt));
      eent->signedd = true;
      return (Type) {
        TypeKindInt,
        { .eent = eent },
        STR_LIT("8"),
      };
    }

    ERROR("There is no support for custom types yet\n");
    exit(1);
  }

  case TypeExprKindPtr: {
    TypePtr *ptr = aalloc(sizeof(TypePtr));
    ptr->points_to = type_from_type_expr(expr.as.ptr->points_to);
    return (Type) { TypeKindPtr, { .ptr = ptr }, STR_LIT("8") };
  }
  }

  ERROR("Unreachable\n");
  exit(1);
}

static void type_print(Type type) {
  switch (type.kind) {
  case TypeKindUnit: {
    printf("()");
  } break;

  case TypeKindInt: {
    if (type.as.eent->signedd)
      putc('s', stdout);
    else
      putc('u', stdout);
    printf("%ld", str_to_i64(type.size) * 8);
  } break;

  case TypeKindPtr: {
    putc('&', stdout);
    type_print(type.as.ptr->points_to);
  } break;

  case TypeKindFunc: {
    printf(STR_FMT"(", STR_ARG(type.as.func->name));

    Def *arg = type.as.func->arg_defs;
    while (arg) {
      type_print(arg->type);
      arg = arg->next;
      if (arg)
        fputs(", ", stdout);
    }
    putc(')', stdout);
  } break;

  }
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
    for (i32 i = 0; i < expr.as.call->args->len; ++i)
      scope_size += expr_scope_size(expr.as.call->args->items[i]);
  } break;

  case ExprKindVar: {
    scope_size = 8 + expr_scope_size(expr.as.var->value);
  } break;

  case ExprKindFunc: break;

  case ExprKindIf: {
    scope_size = expr_scope_size(expr.as.eef->cond);
    scope_size += expr_scope_size(expr.as.eef->body);
    if (expr.as.eef->has_else)
      scope_size += expr_scope_size(expr.as.eef->elze);
  } break;

  case ExprKindWhile: {
    scope_size = expr_scope_size(expr.as.whail->cond);
    scope_size += expr_scope_size(expr.as.whail->body);
  } break;

  case ExprKindRet: {
    if (expr.as.ret->has_result)
      scope_size = expr_scope_size(expr.as.ret->result);
  } break;

  case ExprKindAsm: break;

  case ExprKindDeref: {
    scope_size = expr_scope_size(expr.as.deref->body);
    scope_size += expr_scope_size(expr.as.deref->index);
  } break;
  }

  return scope_size;
}

static Def *checker_lookup_def(Checker *checker, Str target_name) {
  Def *def = checker->defs;
  while (def) {
    if (str_eq(def->name, target_name))
      return def;
    def = def->next;
  }

  return NULL;
}

static Def *checker_lookup_def_typed(Checker *checker, Str target_name, Type target_type) {
  Def *def = checker->defs;
  while (def) {
    if (str_eq(def->name, target_name) &&
        type_eq(def->type, target_type))
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
    for (i32 i = 0; i < expr.as.call->args->len; ++i)
      checker_collect_funcs(checker, expr.as.call->args->items[i]);
  } break;

  case ExprKindVar: {
    checker_collect_funcs(checker, expr.as.var->value);
  } break;

  case ExprKindFunc: {
    Args args = expr.as.func->args;

    Type type = {
      TypeKindFunc,
      { .func = aalloc(sizeof(TypeFunc)) },
      STR_LIT("8"),
    };
    type.as.func->name = expr.as.func->name;
    type.as.func->result_type = type_from_type_expr(expr.as.func->result_type);
    type.as.func->arg_defs = NULL;
    type.as.func->arity = args.len;

    Def *arg_defs_end = NULL;
    for (i32 i = 0; i < args.len; ++i) {
      LL_PREPEND(type.as.func->arg_defs, arg_defs_end, Def);
      arg_defs_end->name = args.items[i].name;
      arg_defs_end->type = type_from_type_expr(args.items[i].type);
    }

    expr.as.func->func_index = checker->funcs.len;
    expr.as.func->def = aalloc(sizeof(Def));
    expr.as.func->def->name = expr.as.func->name;
    expr.as.func->def->type = type;
    expr.as.func->def->is_intrinsic = false;

    if (checker_lookup_def_typed(checker, expr.as.func->name, type)) {
      ERROR("Function `"STR_FMT"` redefined\n",
            STR_ARG(expr.as.func->name));
      checker->has_error = true;
      return;
    }

    Func new_func = {
      .expr = expr.as.func,
      .arg_defs = type.as.func->arg_defs,
      .arity = type.as.func->arity,
    };
    LL_APPEND(checker->defs, Def);
    checker->defs->name = expr.as.func->def->name;
    checker->defs->type = expr.as.func->def->type;
    checker->defs->is_intrinsic = expr.as.func->def->is_intrinsic;
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

  case ExprKindRet: {
    checker_collect_funcs(checker, expr.as.ret->result);
  } break;

  case ExprKindAsm: {
    AsmNode *node = expr.as._asm->nodes;
    while (node) {
      checker_collect_funcs(checker, node->expr);
      node = node->next;
    }
  } break;

  case ExprKindDeref: {
    checker_collect_funcs(checker, expr.as.deref->body);
    checker_collect_funcs(checker, expr.as.deref->index);
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
      eent->signedd = true;
      return (Type) { TypeKindInt, { .eent = eent }, STR_LIT("8") };
    } else if (expr.as.lit->kind == LitKindStr) {
      TypePtr *ptr = aalloc(sizeof(TypePtr));
      ptr->points_to.kind = TypeKindInt;
      ptr->points_to.as.eent = aalloc(sizeof(TypeInt));
      ptr->points_to.as.eent->signedd = true;
      ptr->points_to.size = STR_LIT("8");
      return (Type) { TypeKindPtr, { .ptr = ptr }, STR_LIT("8") };
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
      return (Type) { TypeKindUnit, {0}, STR_LIT("0") };
    }

    return expr.as.ident->def->type;
  }

  case ExprKindVar: {
    Type value_type = checker_type_check_expr(checker, expr.as.var->value);

    if (expr.as.var->has_type) {
      Type var_type = type_from_type_expr(expr.as.var->type);
      if (!type_eq(value_type, var_type)) {
        ERROR("Variable is of type ");
        type_print(var_type);
        fputs(" but initializer is of type ", stdout);
        type_print(value_type);
        putc('\n', stdout);
        checker->has_error = true;
      }
    }

    LL_APPEND(checker->defs, Def);
    checker->defs->name = expr.as.var->name;
    checker->defs->type = value_type;
    expr.as.var->def = checker->defs;

    return value_type;
  }

  case ExprKindCall: {
    ExprBlock *args = expr.as.call->args;

    Type type = {
      TypeKindFunc,
      { .func = aalloc(sizeof(TypeFunc)) },
      STR_LIT("8"),
    };
    type.as.func->name = expr.as.func->name;
    type.as.func->arg_defs = NULL;
    type.as.func->arity = args->len;

    Def *arg_defs_end = NULL;
    for (i32 i = 0; i < args->len; ++i) {
      LL_PREPEND(type.as.func->arg_defs, arg_defs_end, Def);
      arg_defs_end->type = checker_type_check_expr(checker, args->items[i]);
    }

    expr.as.call->def = checker_lookup_def_typed(checker, type.as.func->name, type);
    if (!expr.as.call->def) {
      ERROR("Function with such signature not found: ");
      type_print(type);
      putc('\n', stdout);
      checker->has_error = true;
      return (Type) { TypeKindUnit, {0}, STR_LIT("0") };
    } else if (expr.as.call->def->type.kind != TypeKindFunc) {
      ERROR("Expected function, but got: ");
      type_print(type);
      putc('\n', stdout);
      checker->has_error = true;
      return expr.as.call->def->type;
    }

    return expr.as.call->def->type.as.func->result_type;
  }

  case ExprKindFunc: {
    if (str_eq(expr.as.func->name, STR_LIT("main")))
      checker->found_main = true;

    Def *prev_defs = checker->defs;

    Def *arg_def = checker->funcs.items[expr.as.func->func_index].arg_defs;
    Def *last_arg_def = arg_def;
    while (last_arg_def && last_arg_def->next)
      last_arg_def = last_arg_def->next;
    if (last_arg_def) {
      last_arg_def->next = checker->defs;
      checker->defs = arg_def;
    }

    Type func_type = expr.as.func->def->type;
    Type result_type = func_type.as.func->result_type;

    Type prev_func_result_type = checker->current_func_result_type;
    bool prev_inside_of_func = checker->inside_of_func;
    checker->current_func_result_type = result_type;
    checker->inside_of_func = true;

    Type body_type = checker_type_check_expr(checker, expr.as.func->body);
    if (result_type.kind != TypeKindUnit && !type_eq(body_type, result_type)) {
      ERROR("Current funcion return type is ");
      type_print(result_type);
      fputs(" but got ", stdout);
      type_print(body_type);
      putc('\n', stdout);
      checker->has_error = true;
    }

    checker->inside_of_func = prev_inside_of_func;
    checker->current_func_result_type = prev_func_result_type;

    if (last_arg_def)
      last_arg_def->next = NULL;
    checker->defs = prev_defs;

    return func_type;
  }

  case ExprKindIf: {
    Type cond_type = checker_type_check_expr(checker, expr.as.eef->cond);
    if (cond_type.kind != TypeKindInt && cond_type.kind != TypeKindPtr) {
      ERROR("Expected integer or pointer, but got ");
      type_print(cond_type);
      putc('\n', stdout);
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
  }

  case ExprKindWhile: {
    Type cond_type = checker_type_check_expr(checker, expr.as.whail->cond);
    if (cond_type.kind != TypeKindInt && cond_type.kind != TypeKindPtr) {
      ERROR("Expected integer or pointer, but got ");
      type_print(cond_type);
      putc('\n', stdout);
      checker->has_error = true;
    }

    checker_type_check_expr(checker, expr.as.whail->body);

    return (Type) { TypeKindUnit };
  }

  case ExprKindRet: {
    if (!checker->inside_of_func) {
      ERROR("Could not return: not inside of a function\n");
      checker->has_error = true;
    }

    Type result_type = { TypeKindUnit };
    if (expr.as.ret->has_result)
      result_type = checker_type_check_expr(checker, expr.as.ret->result);

    if (!type_eq(result_type, checker->current_func_result_type)) {
      ERROR("Current return type is ");
      type_print(checker->current_func_result_type);
      fputs(" but this expression returns ", stdout);
      type_print(result_type);
      putc('\n', stdout);
      checker->has_error = true;
    }

    return result_type;
  }

  case ExprKindAsm: {
    AsmNode *node = expr.as._asm->nodes;
    while (node) {
      checker_type_check_expr(checker, node->expr);
      node = node->next;
    }

    return (Type) { TypeKindUnit };
  }

  case ExprKindDeref: {
    Type body_type = checker_type_check_expr(checker, expr.as.deref->body);
    if (body_type.kind != TypeKindPtr) {
      ERROR("Expected pointer, but got ");
      type_print(body_type);
      putc('\n', stderr);
      checker->has_error = true;
      return (Type) { TypeKindUnit };
    }

    Type index_type = checker_type_check_expr(checker, expr.as.deref->index);
    if (index_type.kind != TypeKindInt && index_type.kind != TypeKindPtr) {
      ERROR("Expected integer or pointer, but got");
      type_print(index_type);
      putc('\n', stderr);
      checker->has_error = true;
      return (Type) { TypeKindUnit };
    }

    return body_type.as.ptr->points_to;
  } break;
  }

  ERROR("Unreachable\n");
  exit(1);
}

Metadata type_check(Expr program, Def *intrinsic_defs) {
  Checker checker = {
    .defs = intrinsic_defs,
    .current_func_result_type = { TypeKindUnit },
    .inside_of_func = false,
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
