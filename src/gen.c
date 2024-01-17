#include <stdio.h>
#include <stdlib.h>

#include "gen.h"
#include "log.h"

Str gen_expr_linux_x86_64(StringBuilder *sb, Expr expr, Str target, bool force) {
  Str lhs, rhs, out_reg;

  switch (expr.bin_op->kind) {
  case ExprKindBinOp:
#define EQ(to) str_eq(expr.bin_op->op, to)

    if (EQ(STR("+", 1)) || EQ(STR("-", 1))) {
      lhs = gen_expr_linux_x86_64(sb, expr.bin_op->lhs, target, true);
      rhs = gen_expr_linux_x86_64(sb, expr.bin_op->rhs, STR("rsi", 3), false);

      if (EQ(STR("+", 1)))
        sb_push(sb, "    add ");
      else
        sb_push(sb, "    sub ");
      sb_push_str(sb, lhs);
      sb_push(sb, ", ");
      sb_push_str(sb, rhs);
      sb_push(sb, "\n");
    } else if (EQ(STR("*", 1)) || EQ(STR("/", 1)) || EQ(STR("%", 1))) {
      if (EQ(STR("%", 1)))
        out_reg = STR("rdx", 3);
      else
        out_reg = STR("rax", 3);

      lhs = gen_expr_linux_x86_64(sb, expr.bin_op->lhs, out_reg, true);
      rhs = gen_expr_linux_x86_64(sb, expr.bin_op->rhs, STR("rsi", 3), true);

      if (str_eq(expr.bin_op->op, STR("*", 1)))
        sb_push(sb, "    mul rsi\n");
      else
        sb_push(sb, "    div rsi\n");

      if (!force || str_eq(target, out_reg))
        return out_reg;

      sb_push(sb, "    mov ");
      sb_push_str(sb, target);
      sb_push(sb, ", ");
      sb_push_str(sb, out_reg);
      sb_push(sb, "\n");
    } else {
      ERROR("Unknown operator: ");
      str_print(expr.bin_op->op);
      putc('\n', stdout);
    }

    return target;
  case ExprKindIntLit:
    if (force) {
      sb_push(sb, "    mov ");
      sb_push_str(sb, target);
      sb_push(sb, ", ");
      sb_push_str(sb, expr.int_lit->lit);
      sb_push(sb, "\n");

      return target;
    }

    return expr.int_lit->lit;
  case ExprKindBlock:
    for (i32 i = 0; i + 1 < expr.block->len; ++i)
      gen_expr_linux_x86_64(sb, expr.block->items[i], target, false);

    if (expr.block->len > 0)
      return gen_expr_linux_x86_64(sb, expr.block->items[expr.block->len - 1], target, force);
    if (!force)
      return STR("0", 1);

    sb_push(sb, "    mov, ");
    sb_push_str(sb, target);
    sb_push(sb, "0\n");

    return target;
  case ExprKindIdent:
    ERROR("Not implemented\n");
    exit(1);
  }

  ERROR("Unreachable\n");
  exit(1);
}

char *gen_linux_x86_64(Expr expr) {
  StringBuilder sb = {0};

  sb_push(&sb, "format ELF64 executable\n");
  sb_push(&sb, "segment readable executable\n");

  gen_expr_linux_x86_64(&sb, expr, STR("rdi", 3), true);

  sb_push(&sb, "    mov rax, 60\n");
  sb_push(&sb, "    syscall\n");

  /* sb_push(&sb, "    mov rax, 60\n"); */
  /* sb_push(&sb, "    mov rdi, 0\n"); */
  /* sb_push(&sb, "    syscall\n"); */

  return sb.buffer;
}
