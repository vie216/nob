#include "checker.h"
#include "log.h"
#include "defs.h"
#include "arena.h"

typedef struct {
  Def   *def;
  Funcs  funcs;
  bool   has_error;
  bool   found_main;
  i32    func_scope_size;
} Checker;

static Def *def_lookup(Def *def, Str def_name) {
  while (def) {
    if (str_eq(def->name, def_name))
      return def;
    def = def->next;
  }

  return NULL;
}

static void add_metadata_to_expr(Checker *checker, Expr expr, bool top_level) {
  switch (expr.kind) {
  case ExprKindBlock: {
    Def *prev_def = checker->def;

    for (i32 i = 0; i < expr.as.block->len; ++i)
      add_metadata_to_expr(checker, expr.as.block->items[i], top_level);

    checker->def = prev_def;
  } break;

  case ExprKindLit: break;

  case ExprKindBinOp: {
    add_metadata_to_expr(checker, expr.as.bin_op->lhs, false);
    add_metadata_to_expr(checker, expr.as.bin_op->rhs, false);
  } break;

  case ExprKindIdent: {
    expr.as.ident->def = def_lookup(checker->def, expr.as.ident->ident);

    if (!expr.as.ident->def) {
      ERROR("Undeclared identifier: `");
      str_fprint(stderr, expr.as.ident->ident);
      fputs("`\n", stderr);
      checker->has_error = true;
    }
  } break;

  case ExprKindVar: {
    add_metadata_to_expr(checker, expr.as.var->value, false);

    Def *def = aalloc(sizeof(Def));
    def->name = expr.as.var->name;
    def->size = 8;
    def->next = checker->def;
    checker->def = def;
    expr.as.var->def = def;

    checker->func_scope_size += def->size;
  } break;

  case ExprKindCall: {
    add_metadata_to_expr(checker, expr.as.call->func, false);
  } break;

  case ExprKindFunc: {
    Def *prev_def = def_lookup(checker->def, expr.as.func->name);
    if (prev_def && !top_level) {
      ERROR("Function `");
      str_fprint(stderr, expr.as.func->name);
      fputs("` redefined\n", stderr);
      checker->has_error = true;
    }

    if (str_eq(expr.as.func->name, STR("main", 4)))
      checker->found_main = true;

    Def *def = aalloc(sizeof(Def));
    def->name = expr.as.func->name;
    def->loc = expr.as.func->name;
    def->size = 8;
    def->next = checker->def;
    checker->def = def;
    expr.as.func->def = def;

    i32 prev_func_scope_size = checker->func_scope_size;
    checker->func_scope_size = 0;

    add_metadata_to_expr(checker, expr.as.func->body, false);

    Func func = {
      .expr = expr.as.func,
      .scope_size = checker->func_scope_size,
    };
    DA_APPEND(checker->funcs, func);

    checker->def = def;
    checker->func_scope_size = prev_func_scope_size;
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
    Def *def = aalloc(sizeof(Def));
    def->name = expr.as.var->name;
    def->loc = expr.as.var->name;
    def->size = 8;
    def->next = checker->def;
    checker->def = def;
  } else {
    ERROR("Only function definitions and blocks are supported on the top level for now\n");
    checker->has_error = true;
  }
}

Metadata add_metadata(Expr program) {
  Checker checker = {0};

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
    .def = checker.def,
  };
}
