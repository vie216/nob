#include <stdio.h>
#include <stdlib.h>

#include "gen.h"
#include "log.h"

typedef struct {
  Str name, data;
} DB;

typedef struct {
  DB *items;
  i32 len, cap;
} DBs;

typedef struct {
  StringBuilder sb;
  DBs           dbs;
} Generator;

Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Str target, bool force) {
  Str lhs, rhs, out_reg;
  DB db;

  switch (expr.kind) {
  case ExprKindBinOp:
#define EQ(to) str_eq(expr.as.bin_op->op, to)

    if (EQ(STR("+", 1)) || EQ(STR("-", 1))) {
      lhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->lhs, target, true);
      rhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->rhs, STR("rsi", 3), false);

      if (EQ(STR("+", 1)))
        sb_push(&gen->sb, "    add ");
      else
        sb_push(&gen->sb, "    sub ");
      sb_push_str(&gen->sb, lhs);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, rhs);
      sb_push(&gen->sb, "\n");
    } else if (EQ(STR("*", 1)) || EQ(STR("/", 1)) || EQ(STR("%", 1))) {
      if (EQ(STR("%", 1)))
        out_reg = STR("rdx", 3);
      else
        out_reg = STR("rax", 3);

      lhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->lhs, out_reg, true);
      rhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->rhs, STR("rsi", 3), true);

      if (str_eq(expr.as.bin_op->op, STR("*", 1)))
        sb_push(&gen->sb, "    mul rsi\n");
      else
        sb_push(&gen->sb, "    div rsi\n");

      if (!force || str_eq(target, out_reg))
        return out_reg;

      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, out_reg);
      sb_push(&gen->sb, "\n");
    } else {
      ERROR("Unknown operator: ");
      str_print(expr.as.bin_op->op);
      putc('\n', stdout);
    }

    return target;
  case ExprKindIntLit:
    if (!force)
      return expr.as.int_lit->lit;

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, expr.as.int_lit->lit);
    sb_push(&gen->sb, "\n");

    return target;
  case ExprKindBlock:
    for (i32 i = 0; i + 1 < expr.as.block->len; ++i)
      gen_expr_linux_x86_64(gen, expr.as.block->items[i], target, false);

    if (expr.as.block->len > 0)
      return gen_expr_linux_x86_64(gen, expr.as.block->items[expr.as.block->len - 1], target, force);
    if (!force)
      return STR("0", 1);

    sb_push(&gen->sb, "    mov, ");
    sb_push_str(&gen->sb, target);
    sb_push(&gen->sb, ", 0\n");

    return target;
  case ExprKindIdent:
    ERROR("Not implemented\n");
    exit(1);
  case ExprKindCall:
    sb_push(&gen->sb, "    call ");
    sb_push_str(&gen->sb, expr.as.call->name);
    sb_push(&gen->sb, "\n");

    if (!force)
      return STR("rax", 3);

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target);
    sb_push(&gen->sb, ", rax\n");
    return target;
  case ExprKindStrLit:
    db = (DB) { STR(malloc(6), 6), expr.as.str_lit->lit };
    db.name.len = sprintf(db.name.ptr, "db_%d", gen->dbs.len);
    DA_APPEND(gen->dbs, db);

    if (!force)
      return db.name;

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, db.name);
    sb_push(&gen->sb, "\n");

    return target;
  }

  ERROR("Unreachable\n");
  exit(1);
}

char *gen_linux_x86_64(Expr expr) {
  Generator gen = {0};

  sb_push(&gen.sb, "format ELF64 executable\n");
  sb_push(&gen.sb, "entry _start\n");
  sb_push(&gen.sb, "segment readable executable\n");
  sb_push(&gen.sb, "_start:\n");

  gen_expr_linux_x86_64(&gen, expr, STR("rdi", 3), true);

  sb_push(&gen.sb, "    mov rax, 60\n");
  sb_push(&gen.sb, "    syscall\n");

  sb_push(&gen.sb, "segment readable\n");
  for (i32 i = 0; i < gen.dbs.len; ++i) {
    sb_push_str(&gen.sb, gen.dbs.items[i].name);
    sb_push(&gen.sb, ": db ");
    sb_push_str(&gen.sb, gen.dbs.items[i].data);
    sb_push(&gen.sb, ", 0\n");
  }

  /* sb_push(&sb, "    mov rax, 60\n"); */
  /* sb_push(&sb, "    mov rdi, 0\n"); */
  /* sb_push(&sb, "    syscall\n"); */

  return gen.sb.buffer;
}
