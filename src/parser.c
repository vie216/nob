#include <stdlib.h>

#include "parser.h"

Expr parse_program(Str source) {
  ExprIntLit *lit0, *lit1, *lit2;
  ExprBinOp *bin_op0, *bin_op1;
  Expr expr0, expr1, expr2, expr3, expr4;

  (void) source;

  lit0 = malloc(sizeof(ExprIntLit));
  lit1 = malloc(sizeof(ExprIntLit));
  lit2 = malloc(sizeof(ExprIntLit));
  bin_op0 = malloc(sizeof(ExprBinOp));
  bin_op1 = malloc(sizeof(ExprBinOp));

  *lit0 = (ExprIntLit) { .kind = ExprKindIntLit, .lit = { .ptr = "35", .len = 2 } };
  expr0.int_lit = lit0;
  *lit1 = (ExprIntLit) { .kind = ExprKindIntLit, .lit = { .ptr = "34", .len = 2 } };
  expr1.int_lit = lit1;
  *lit2 = (ExprIntLit) { .kind = ExprKindIntLit, .lit = { .ptr = "2", .len = 1 } };
  expr2.int_lit = lit2;

  *bin_op0 = (ExprBinOp) { .kind = ExprKindBinOp, .op = { .ptr = "+", .len = 1 }, .lhs = expr0, .rhs = expr1 };
  expr3.bin_op = bin_op0;
  *bin_op1 = (ExprBinOp) { .kind = ExprKindBinOp, .op = { .ptr = "*", .len = 1 }, .lhs = expr3, .rhs = expr2 };
  expr4.bin_op = bin_op1;

  return expr4;
}
