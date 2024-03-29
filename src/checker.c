#include "checker.h"
#include "log.h"
#include "defs.h"
#include "arena.h"

typedef struct {
  Def   *defs;
  Funcs  funcs;
  bool   has_error;
  bool   found_main;
  i32    func_scope_size;
} Checker;

static Def *defs_lookup(Def *defs, Str def_name) {
  while (defs) {
    if (str_eq(defs->name, def_name))
      return defs;
    defs = defs->next;
  }

  return NULL;
}

static void add_metadata_to_expr(Checker *checker, Expr expr, bool top_level) {
  switch (expr.kind) {
  case ExprKindBlock: {
    Def *prev_defs = checker->defs;

    for (i32 i = 0; i < expr.as.block->len; ++i)
      add_metadata_to_expr(checker, expr.as.block->items[i], top_level);

    checker->defs = prev_defs;
  } break;

  case ExprKindLit: break;

  case ExprKindIdent: {
    expr.as.ident->def = defs_lookup(checker->defs, expr.as.ident->ident);
    if (!expr.as.ident->def) {
      ERROR("Undeclared identifier: `");
      str_fprint(stderr, expr.as.ident->ident);
      fputs("`\n", stderr);
      checker->has_error = true;
    }
  } break;

  case ExprKindVar: {
    add_metadata_to_expr(checker, expr.as.var->value, false);

    LL_APPEND(checker->defs, Def);
    expr.as.var->def = checker->defs;
    checker->defs->name = expr.as.var->name;
    checker->defs->size = 8;

    checker->func_scope_size += checker->defs->size;
  } break;

  case ExprKindCall: {
    add_metadata_to_expr(checker, expr.as.call->func, false);

    ExprBlock *args = expr.as.call->args;
    for (i32 i = 0; i < args->len; ++i)
      add_metadata_to_expr(checker, args->items[i], false);
  } break;

  case ExprKindFunc: {
    Def *def = defs_lookup(checker->defs, expr.as.func->name);
    if (def && !top_level) {
      ERROR("Function `");
      str_fprint(stderr, expr.as.func->name);
      fputs("` redefined\n", stderr);
      checker->has_error = true;
    }

    if (str_eq(expr.as.func->name, STR("main", 4)))
      checker->found_main = true;

    LL_APPEND(checker->defs, Def);
    expr.as.func->def = checker->defs;
    checker->defs->name = expr.as.func->name;
    checker->defs->loc = expr.as.func->name;
    checker->defs->size = 8;

    Def *prev_defs = checker->defs;
    i32 prev_func_scope_size = checker->func_scope_size;
    checker->func_scope_size = 0;

    add_metadata_to_expr(checker, expr.as.func->body, false);

    Func func = {
      .expr = expr.as.func,
      .scope_size = checker->func_scope_size,
    };
    DA_APPEND(checker->funcs, func);

    checker->func_scope_size = prev_func_scope_size;
    checker->defs = prev_defs;
  } break;

  case ExprKindIf: {
    add_metadata_to_expr(checker, expr.as.eef->cond, false);
    add_metadata_to_expr(checker, expr.as.eef->body, false);
    if (expr.as.eef->has_else)
      add_metadata_to_expr(checker, expr.as.eef->elze, false);
  } break;

  default: {
    ERROR("Unreachable\n");
    exit(1);
  }
  }
}

static void verify_top_level_expr(Checker *checker, Expr expr) {
  if (expr.kind == ExprKindBlock) {
    for (i32 i = 0; i < expr.as.block->len; ++i)
      verify_top_level_expr(checker, expr.as.block->items[i]);
  } else if (expr.kind == ExprKindFunc) {
    LL_APPEND(checker->defs, Def);
    checker->defs->name = expr.as.var->name;
    checker->defs->loc = expr.as.var->name;
    checker->defs->size = 8;
  } else {
    ERROR("Only function definitions and blocks are supported on the top level for now\n");
    checker->has_error = true;
  }
}

Metadata add_metadata(Expr program, Def *intrinsic_defs) {
  Checker checker = {0};
  checker.defs = intrinsic_defs;

  verify_top_level_expr(&checker, program);
  add_metadata_to_expr(&checker, program, true);

  if (!checker.found_main) {
    ERROR("Function `main` is required\n");
    checker.has_error = true;
  }

  if (checker.has_error)
    exit(1);

  return (Metadata) {
    .funcs = checker.funcs,
    .defs = checker.defs,
  };
}
