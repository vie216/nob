#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "defs.h"

typedef struct {
  char *ptr;
  i32   len;
} Str;

Str str_new(char *str) {
  return (Str) { .ptr = str, .len = strlen(str) };
}

typedef enum {
  ExprKindBinOp = 0,
  ExprKindIntLit,
} ExprKind;

typedef struct ExprBinOp ExprBinOp;
typedef struct ExprIntLit ExprIntLit;

typedef union {
  ExprBinOp  *bin_op;
  ExprIntLit *int_lit;
} Expr;

struct ExprBinOp {
  ExprKind kind;
  Str      op;
  Expr     lhs;
  Expr     rhs;
};

struct ExprIntLit {
  ExprKind  kind;
  Str       lit;
};

typedef struct {
  char *buffer;
  i32   cap;
  i32   len;
} StringBuilder;

void sb_push_str(StringBuilder *sb, Str str) {
  if (str.len > sb->cap - sb->len) {
    if (sb->cap) {
      while (str.len > sb->cap - sb->len)
        sb->cap *= 2;

      sb->buffer = realloc(sb->buffer, sb->cap + 1);
    } else {
      sb->cap += str.len;
      sb->buffer = malloc(sb->cap + 1);
    }
  }

  memmove(sb->buffer + sb->len, str.ptr, str.len);
  sb->len += str.len;
  sb->buffer[sb->len] = '\0';
}

void sb_push(StringBuilder *sb, char *str) {
  sb_push_str(sb, str_new(str));
}

Str gen_expr_linux_x86_64(StringBuilder *sb, Expr *expr, Str target, bool force) {
  Str lhs, rhs;

  switch (expr->bin_op->kind) {
  case ExprKindBinOp:
    lhs = gen_expr_linux_x86_64(sb, &expr->bin_op->lhs, target, true);
    rhs = gen_expr_linux_x86_64(sb, &expr->bin_op->rhs, str_new("rsi"), false);

    sb_push(sb, "    add ");
    sb_push_str(sb, lhs);
    sb_push(sb, ", ");
    sb_push_str(sb, rhs);
    sb_push(sb, "\n");
    break;
  case ExprKindIntLit:
    if (force) {
      sb_push(sb, "    mov ");
      sb_push_str(sb, target);
      sb_push(sb, ", ");
      sb_push_str(sb, expr->int_lit->lit);
      sb_push(sb, "\n");
      return target;
    }

    return expr->int_lit->lit;
  }
}

char *gen_linux_x86_64(Expr expr) {
  StringBuilder sb = {0};

  sb_push(&sb, "format ELF64 executable\n");
  sb_push(&sb, "segment readable executable\n");

  gen_expr_linux_x86_64(&sb, &expr, str_new("rdi"), false);

  sb_push(&sb, "    mov rax, 60\n");
  sb_push(&sb, "    syscall\n");

  /* sb_push(&sb, "    mov rax, 60\n"); */
  /* sb_push(&sb, "    mov rdi, 0\n"); */
  /* sb_push(&sb, "    syscall\n"); */

  return sb.buffer;
}

int main() {
  FILE *output_file;
  ExprIntLit lit0, lit1;
  ExprBinOp bin_op;
  Expr expr0, expr1, expr2;

  lit0 = (ExprIntLit) { .kind = ExprKindIntLit, .lit = { .ptr = "35", .len = 2 } };
  expr0.int_lit = &lit0;
  lit1 = (ExprIntLit) { .kind = ExprKindIntLit, .lit = { .ptr = "34", .len = 2 } };
  expr1.int_lit = &lit1;
  bin_op = (ExprBinOp) { .kind = ExprKindBinOp, .op = { .ptr = "+", .len = 1 }, .lhs = expr0, .rhs = expr1 };
  expr2.bin_op = &bin_op;

  output_file = fopen("test.asm", "w");
  fprintf(output_file, gen_linux_x86_64(expr2));
  fclose(output_file);
}
