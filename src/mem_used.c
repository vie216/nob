#include "defs.h"
#include "log.h"
#include "mem_used.h"

static Str reg_names[] = { STR_LIT("rbx"), STR_LIT("r12"), STR_LIT("r13"),
                           STR_LIT("r14"), STR_LIT("r15") };

static void mem_used_use_reg(MemUsed *mem_used) {
  if (mem_used->regs_used >= (i32) ARRAY_LEN(reg_names)) {
    ERROR("Exceeded amount of available registers\n");
    exit(1);
  }

  if (++mem_used->regs_used > mem_used->max_regs_used)
    mem_used->max_regs_used = mem_used->regs_used;
}

void mem_used_count_in_expr(MemUsed *mem_used, Expr expr, bool target_is_return) {
  switch (expr.kind) {
  case ExprKindLit: break;

  case ExprKindBlock: {
    for (i32 i = 0; i + 1 < expr.as.block->len; ++i)
      mem_used_count_in_expr(mem_used, expr.as.block->items[i], false);
    if (expr.as.block->len > 0)
      mem_used_count_in_expr(mem_used,
                             expr.as.block->items[expr.as.block->len - 1],
                             target_is_return);
  } break;

  case ExprKindIdent: break;

  case ExprKindCall: {
    if (expr.as.call->def->is_intrinsic) {
      if (expr.as.call->args->len == 2) {
        bool preserve_rax_on_rhs_call = target_is_return &&
                                        expr.as.call->args->items[1].kind == ExprKindCall;

        if (str_eq(expr.as.call->name, STR_LIT("+")) ||
            str_eq(expr.as.call->name, STR_LIT("-")) ||
            str_eq(expr.as.call->name, STR_LIT("*")) ||
            str_eq(expr.as.call->name, STR_LIT("/")) ||
            str_eq(expr.as.call->name, STR_LIT("%")) ||
            str_eq(expr.as.call->name, STR_LIT("%")) ||
            str_eq(expr.as.call->name, STR_LIT("%"))) {
          if (preserve_rax_on_rhs_call)
            mem_used_use_reg(mem_used);
          mem_used_use_reg(mem_used);

          mem_used_count_in_expr(mem_used, expr.as.call->args->items[0], target_is_return);
          mem_used_count_in_expr(mem_used, expr.as.call->args->items[1], false);

          mem_used->regs_used -= 1 + preserve_rax_on_rhs_call;
        } else if(str_eq(expr.as.call->name, STR_LIT("="))) {
            mem_used_count_in_expr(mem_used, expr.as.call->args->items[1], target_is_return);
        } else {
          ERROR("No code generation for `"STR_FMT"` intrinsic was fount\n",
                STR_ARG(expr.as.call->name));
        }
      } else if (expr.as.call->args->len == 1) {
        if(str_eq(expr.as.call->name, STR_LIT("*"))) {
          mem_used_use_reg(mem_used);
          mem_used_count_in_expr(mem_used, expr.as.call->args->items[0], target_is_return);
          --mem_used->regs_used;
        } else {
          ERROR("No code generation for `"STR_FMT"` intrinsic was fount\n",
                STR_ARG(expr.as.call->name));
        }
      } else {
        ERROR("Wrong arguments count for `"STR_FMT"` intrinsic: expected one or two, but got %d\n",
              STR_ARG(expr.as.call->name), expr.as.call->args->len);
        exit(1);

      }
     } else {
      if (mem_used->max_args_count_in_subcall < expr.as.call->args->len)
        mem_used->max_args_count_in_subcall = expr.as.call->args->len;

      if (mem_used->max_arg_regs_used < expr.as.call->args->len)
        mem_used->max_arg_regs_used = expr.as.call->args->len;

      i32 args_count = expr.as.call->args->len;
      if (args_count > mem_used->func->arity)
        args_count = mem_used->func->arity;

      for (i32 i = 0; i < expr.as.call->args->len; ++i)
        mem_used_count_in_expr(mem_used, expr.as.call->args->items[i], false);
    }
  } break;

  case ExprKindVar: {
    mem_used->scope_size += 8;
    mem_used_count_in_expr(mem_used, expr.as.var->value, target_is_return);
  } break;

  case ExprKindFunc: break;

  case ExprKindIf: {
    mem_used_count_in_expr(mem_used, expr.as.eef->body,
                           target_is_return && expr.as.eef->has_else);
    if (expr.as.eef->has_else)
      mem_used_count_in_expr(mem_used, expr.as.eef->elze, target_is_return);
  } break;

  case ExprKindWhile: {
    mem_used_count_in_expr(mem_used, expr.as.whail->body, false);
  } break;

  case ExprKindAsm: break;

  case ExprKindRet: {
    if (expr.as.ret->has_result)
      mem_used_count_in_expr(mem_used, expr.as.ret->result, true);
  } break;

  case ExprKindDeref: {
    mem_used_use_reg(mem_used);
    mem_used_count_in_expr(mem_used, expr.as.deref->body, target_is_return);
    mem_used_count_in_expr(mem_used, expr.as.deref->index, false);
    --mem_used->regs_used;
  } break;


  case ExprKindUse: break;

  case ExprKindArray: break;

  default: {
    ERROR("Unreachable\n");
    exit(1);
  }
  }
}
