#include "checker.h"
#include "log.h"
#include "defs.h"

typedef struct {
  Str  name;
  Expr expr;
} Binding;

typedef struct {
  Binding *items;
  i32      len, cap;
} Bindings;

typedef struct {
  Functions funcs;
  Bindings  binds;
  bool      has_error;
  bool      found_main;
  i32       func_scope_size;
} Checker;

static Binding *bindings_lookup(Bindings *binds, Str bind_name) {
  for (i32 i = binds->len - 1; i >= 0; --i)
    if (str_eq(binds->items[i].name, bind_name))
      return binds->items + i;

  return NULL;
}

static i32 type_size(void) {
  return 8;
}

static void add_metadata_to_expr(Checker *checker, Expr *expr, bool top_level) {
  switch (expr->kind) {
  case ExprKindBlock: {
    i32 prev_binds_len = checker->binds.len;

    for (i32 i = 0; i < expr->as.block->len; ++i)
      add_metadata_to_expr(checker, expr->as.block->items + i, top_level);

    checker->binds.len = prev_binds_len;
  } break;

  case ExprKindLit: break;

  case ExprKindBinOp: {
    add_metadata_to_expr(checker, &expr->as.bin_op->lhs, false);
    add_metadata_to_expr(checker, &expr->as.bin_op->rhs, false);
  } break;

  case ExprKindIdent: {
    Binding *bind = bindings_lookup(&checker->binds,
                                    expr->as.ident->ident);

    if (bind) {
      expr->as.ident->target_name = bind->name;
      expr->as.ident->target_expr = bind->expr;
    } else {
      ERROR("Undeclared identifier: `");
      str_fprint(stderr, expr->as.ident->ident);
      fputs("`\n", stderr);
      checker->has_error = true;
    }
  } break;

  case ExprKindVar: {
    expr->as.var->size = type_size();
    checker->func_scope_size += expr->as.var->size;

    add_metadata_to_expr(checker, &expr->as.var->value, false);

    Binding bind = {
      expr->as.var->name,
      *expr,
    };
    DA_APPEND(checker->binds, bind);
  } break;

  case ExprKindCall: {
    add_metadata_to_expr(checker, &expr->as.call->func, false);
  } break;

  case ExprKindFunc: {
    Binding *prev_bind = bindings_lookup(&checker->binds, expr->as.func->name);
    if (prev_bind && !top_level) {
      ERROR("Function `");
      str_fprint(stderr, expr->as.func->name);
      fputs("` redefined\n", stderr);
      checker->has_error = true;
    }

    if (str_eq(expr->as.func->name, STR("main", 4)))
      checker->found_main = true;

    DA_APPEND(checker->funcs, expr->as.func);

    Binding bind = {
      expr->as.func->name,
      *expr,
    };
    DA_APPEND(checker->binds, bind);

    i32 prev_func_scope_size = checker->func_scope_size;
    checker->func_scope_size = 0;
    i32 prev_binds_len = checker->binds.len;

    add_metadata_to_expr(checker, &expr->as.func->body, false);

    checker->binds.len = prev_binds_len;
    expr->as.func->scope_size = checker->func_scope_size;
    checker->func_scope_size = prev_func_scope_size;
  } break;

  default: {
    ERROR("Not implemented\n");
    exit(1);
  }
  }
}

static void verify_top_level_items(Checker *checker, Expr *expr) {
  if (expr->kind == ExprKindBlock) {
    for (i32 i = 0; i < expr->as.block->len; ++i)
      verify_top_level_items(checker, expr->as.block->items + i);
  } else if (expr->kind == ExprKindFunc) {
    Binding bind = {
      expr->as.var->name,
      *expr,
    };
    DA_APPEND(checker->binds, bind);
  } else {
    ERROR("Only function definitions are supported on the top level for now\n");
    checker->has_error = true;
  }
}

Functions add_metadata(Expr *program) {
  Checker checker = {0};

  verify_top_level_items(&checker, program);
  add_metadata_to_expr(&checker, program, true);
  free(checker.binds.items);

  if (!checker.found_main) {
    ERROR("Function `main` is required\n");
    checker.has_error = true;
  }

  if (checker.has_error)
    exit(1);

  return checker.funcs;
}
