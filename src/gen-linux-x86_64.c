#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "gen-linux-x86_64.h"
#include "log.h"
#include "arena.h"
#include "hash.h"

#define TARGET(loc, strict) ((Target) { loc, strict })

typedef struct {
  Str  str;
  bool strict;
} Target;

typedef struct {
  Func *func;
  i32   max_regs_used;
  i32   regs_used;
  i32   max_arg_regs_used;
  i32   arg_regs_used;
} MemUsed;

static Str reg_names[] = { STR_LIT("rbx"), STR_LIT("r12"), STR_LIT("r13"),
                           STR_LIT("r14"), STR_LIT("r15") };

static Str arg_reg_names[] = { STR_LIT("rdi"), STR_LIT("rsi"), STR_LIT("rdx"),
                               STR_LIT("rcx"), STR_LIT("r8"), STR_LIT("r9") };

static void mem_used_use_reg(MemUsed *mem_used) {
  if (mem_used->regs_used >= (i32) ARRAY_LEN(reg_names)) {
    ERROR("Exceeded amount of available registers\n");
    exit(1);
  }

  if (++mem_used->regs_used > mem_used->max_regs_used)
    mem_used->max_regs_used = mem_used->regs_used;
}

static Str gen_stack_alloc(Generator *gen, i32 amount) {
  StringBuilder sb = {0};
  sb_push(&sb, "qword [rsp+");
  sb_push_i32(&sb, gen->stack_used);
  sb_push(&sb, "]");

  gen->stack_used += amount;

  return sb_to_str(sb);
}

static Str gen_var_loc(Generator *gen, Str name) {
  i32 offset = 0;

  for (i32 i = gen->vars.len - 1; i >= 0; --i) {
    if (str_eq(gen->vars.items[i].name, name)) {
      offset = gen->vars.items[i].offset;
      break;
    }
  }

  StringBuilder sb = {0};
  sb_push(&sb, "qword [rsp+");
  sb_push_i32(&sb, offset);
  sb_push(&sb, "]");

  return sb_to_str(sb);
}

static void def_mangle(Def *def) {
  i32 hash = hash_type(def->type);
  StringBuilder temp_sb = {0};
  sb_push_str(&temp_sb, def->name);
  sb_push_char(&temp_sb, '@');
  sb_push_i32(&temp_sb, hash < 0 ? -hash : hash);
  def->loc = sb_to_str(temp_sb);
}

static Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Target target);

static Str gen_intrinsic_linux_x86_64(Generator *gen, Str name, ExprBlock *args, Target target) {
  bool preserve_rax_on_rhs_call = str_eq(target.str, STR_LIT("rax")) &&
                                  args->items[1].kind == ExprKindCall;

  if (str_eq(name, STR_LIT("+")) ||
      str_eq(name, STR_LIT("-")) ||
      str_eq(name, STR_LIT("*"))) {
    Str lhs_loc = target.str;
    if (preserve_rax_on_rhs_call)
      lhs_loc = reg_names[gen->regs_used++];
    Str lhs = gen_expr_linux_x86_64(gen, args->items[0], TARGET(lhs_loc, true));

    Str rhs_loc = reg_names[gen->regs_used++];
    Str rhs = gen_expr_linux_x86_64(gen, args->items[1], TARGET(rhs_loc, false));

    gen->regs_used -= 1 + preserve_rax_on_rhs_call;

    if (str_eq(name, STR_LIT("+")))
      sb_push(&gen->sb, "\tadd ");
    else if (str_eq(name, STR_LIT("-")))
      sb_push(&gen->sb, "\tsub ");
    else
      sb_push(&gen->sb, "\timul ");
    sb_push_str(&gen->sb, lhs);
    sb_push(&gen->sb, ",");
    sb_push_str(&gen->sb, rhs);
    sb_push(&gen->sb, "\n");

    if (preserve_rax_on_rhs_call) {
      sb_push(&gen->sb, "\tmov rax,");
      sb_push_str(&gen->sb, lhs_loc);
      sb_push(&gen->sb, "\n");
    }

    return lhs;
  } else if (str_eq(name, STR_LIT("/")) ||
             str_eq(name, STR_LIT("%"))) {
    Str lhs_loc = STR_LIT("rax");
    if (preserve_rax_on_rhs_call)
      lhs_loc = reg_names[gen->regs_used++];
    gen_expr_linux_x86_64(gen, args->items[0], TARGET(lhs_loc, true));

    Str rhs_loc = reg_names[gen->regs_used++];
    Str rhs = gen_expr_linux_x86_64(gen, args->items[1], TARGET(rhs_loc, true));

    gen->regs_used -= 1 + preserve_rax_on_rhs_call;

    if (preserve_rax_on_rhs_call) {
      sb_push(&gen->sb, "\tmov rax,");
      sb_push_str(&gen->sb, lhs_loc);
      sb_push(&gen->sb, "\n");
    }

    sb_push(&gen->sb, "\tcdq\n");
    sb_push(&gen->sb, "\tidiv ");
    sb_push_str(&gen->sb, rhs);
    sb_push(&gen->sb, "\n");

    Str target_loc = str_eq(name, STR_LIT("/")) ? STR_LIT("rax") : STR_LIT("rdx");

    if (target.strict && !str_eq(target.str, target_loc)) {
      sb_push(&gen->sb, "\tmov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ",");
      sb_push_str(&gen->sb, target_loc);
      sb_push(&gen->sb, "\n");

      return target.str;
    }

    return target_loc;
  } else if (str_eq(name, STR_LIT("="))) {
    Str var_name = args->items[0].as.ident->ident;
    Str var_loc = gen_var_loc(gen, var_name);

    Str loc = gen_expr_linux_x86_64(gen, args->items[1], target);

    if (!str_eq(loc, var_loc)) {
      sb_push(&gen->sb, "\tmov ");
      sb_push_str(&gen->sb, var_loc);
      sb_push(&gen->sb, ",");
      sb_push_str(&gen->sb, loc);
      sb_push(&gen->sb, "\n");

      return target.str;
    }

    return var_loc;
  } else if (str_eq(name, STR_LIT("==")) ||
             str_eq(name, STR_LIT("!="))) {
    Str lhs_loc = target.str;
    if (preserve_rax_on_rhs_call)
      lhs_loc = reg_names[gen->regs_used++];
    Str lhs = gen_expr_linux_x86_64(gen, args->items[0], TARGET(lhs_loc, true));
    Str rhs_loc = reg_names[gen->regs_used++];
    Str rhs = gen_expr_linux_x86_64(gen, args->items[1], TARGET(rhs_loc, false));

    gen->regs_used -= 1 + preserve_rax_on_rhs_call;

    sb_push(&gen->sb, "\tcmp ");
    sb_push_str(&gen->sb, lhs);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, rhs);
    sb_push(&gen->sb, "\n");
    if (str_eq(name, STR_LIT("==")))
      sb_push(&gen->sb, "\tsete al\n");
    else
      sb_push(&gen->sb, "\tsetne al\n");
    sb_push(&gen->sb, "\tmovzx ");
    sb_push_str(&gen->sb, lhs);
    sb_push(&gen->sb, ", al\n");

    return lhs;
  }

  ERROR("Unreachable\n");
  exit(1);
}

static Str gen_lit_linux_x86_64(Generator *gen, ExprLit *lit, Target target) {
  if (lit->kind == LitKindInt) {
    if (!target.strict)
      return lit->lit;

    if (str_eq(lit->lit, STR_LIT("0"))) {
      sb_push(&gen->sb, "\txor ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ",");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, "\n");
    } else {
      sb_push(&gen->sb, "\tmov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ",");
      sb_push_str(&gen->sb, lit->lit);
      sb_push(&gen->sb, "\n");
    }

    return target.str;
  } else if (lit->kind == LitKindStr) {
    StringBuilder sb = {0};
    sb_push(&sb, "db");
    sb_push_i32(&sb, gen->strings.len);
    Str db_name = sb_to_str(sb);

    DA_APPEND(gen->strings, lit->lit);

    if (!target.strict)
      return db_name;

    sb_push(&gen->sb, "\tmov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ",");
    sb_push_str(&gen->sb, db_name);
    sb_push(&gen->sb, "\n");

    return target.str;
  }

  ERROR("Unreachable\n");
  exit(1);
}

static Str gen_block_linux_x86_64(Generator *gen, ExprBlock *block, Target target) {
  i32 prev_stack_used = gen->stack_used;

  for (i32 i = 0; i + 1 < block->len; ++i)
    gen_expr_linux_x86_64(gen, block->items[i], TARGET(STR_LIT("rax"), false));

  Str loc = STR_LIT("rax");

  if (block->len > 0) {
    Expr last = block->items[block->len - 1];
    loc = gen_expr_linux_x86_64(gen, last, target);
  }

  gen->stack_used = prev_stack_used;

  return loc;
}

static Str gen_ident_linux_x86_64(Generator *gen, ExprIdent *ident, Target target) {
  if (!target.strict || str_eq(target.str, ident->def->loc))
    return ident->def->loc;

  sb_push(&gen->sb, "\tmov ");
  sb_push_str(&gen->sb, target.str);
  sb_push(&gen->sb, ",");
  sb_push_str(&gen->sb, ident->def->loc);
  sb_push(&gen->sb, "\n");

  return target.str;
}

static Str gen_call_linux_x86_64(Generator *gen, ExprCall *call, Target target) {
  if (call->def->is_intrinsic)
    return gen_intrinsic_linux_x86_64(gen, call->name, call->args, target);

  if (call->args->len > (i32) ARRAY_LEN(arg_reg_names)) {
    ERROR("Exceeded amount of registers for function arguments\n");
    INFO("TODO: use stack when this happens\n");
    exit(1);
  }

  if (call->args->len > gen->ctx.max_arg_regs_used)
    gen->ctx.max_arg_regs_used = call->args->len;

  i32 args_count = call->args->len;
  if (args_count > gen->ctx.func->arity)
    args_count = gen->ctx.func->arity;

  for (i32 i = 0; i < args_count; ++i) {
    sb_push(&gen->sb, "\tmov ");
    sb_push_str(&gen->sb, gen_stack_alloc(gen, 8));
    sb_push(&gen->sb, ",");
    sb_push_str(&gen->sb, arg_reg_names[i]);
    sb_push(&gen->sb, "\n");
  }

  gen->stack_used += args_count * 8;

  i32 prev_stack_used = gen->stack_used;

  for (i32 i = 0; i < call->args->len; ++i)
    gen_expr_linux_x86_64(gen, call->args->items[i], TARGET(arg_reg_names[i], true));

  sb_push(&gen->sb, "\tcall ");
  sb_push_str(&gen->sb, call->def->loc);
  sb_push(&gen->sb, "\n");

  if (target.strict && !str_eq(target.str, STR_LIT("rax"))) {
    sb_push(&gen->sb, "\tmov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ",rax\n");
  }

  gen->stack_used -= prev_stack_used;

  for (i32 i = 0; i < args_count; ++i) {
    sb_push(&gen->sb, "\tmov ");
    sb_push_str(&gen->sb, arg_reg_names[i]);
    sb_push(&gen->sb, ",");
    sb_push_str(&gen->sb, gen_stack_alloc(gen, 8));
    sb_push(&gen->sb, "\n");
  }

  if (target.strict)
    return target.str;
  return STR_LIT("rax");
}

static Str gen_var_linux_x86_64(Generator *gen, ExprVar *var, Target target) {
  var->def->loc = gen_stack_alloc(gen, 8);

  gen->stack_used -= 8;

  Var new_var = { var->name, gen->stack_used };
  DA_APPEND(gen->vars, new_var);

  Str value = gen_expr_linux_x86_64(gen, var->value, target);

  gen->stack_used -= 8;

  if (!str_eq(value, var->def->loc)) {
    sb_push(&gen->sb, "\tmov ");
    sb_push_str(&gen->sb, var->def->loc);
    sb_push(&gen->sb, ",");
    sb_push_str(&gen->sb, value);
    sb_push(&gen->sb, "\n");
  }

  return value;
}

static Str gen_func_linux_x86_64(Generator *gen, ExprFunc *func, Target target) {
  if (!target.strict)
    return func->def->loc;

  sb_push(&gen->sb, "\tmov ");
  sb_push_str(&gen->sb, target.str);
  sb_push(&gen->sb, ",");
  sb_push_str(&gen->sb, func->def->loc);
  sb_push(&gen->sb, "\n");

  return target.str;
}

static Str gen_if_linux_x86_64(Generator *gen, ExprIf *eef, Target target) {
  Str cond = gen_expr_linux_x86_64(gen, eef->cond, TARGET(target.str, true));
  i32 else_id = gen->label_count++;
  i32 next_id = gen->label_count++;

  sb_push(&gen->sb, "\tcmp ");
  sb_push_str(&gen->sb, cond);
  sb_push(&gen->sb, ",0\n");
  sb_push(&gen->sb, "\tje .l");
  sb_push_i32(&gen->sb, else_id);
  sb_push(&gen->sb, "\n");

  gen_expr_linux_x86_64(gen, eef->body, TARGET(target.str, eef->has_else));

  if (eef->has_else) {
    sb_push(&gen->sb, "\tjmp .l");
    sb_push_i32(&gen->sb, next_id);
    sb_push(&gen->sb, "\n");
  }

  sb_push(&gen->sb, ".l");
  sb_push_i32(&gen->sb, else_id);
  sb_push(&gen->sb, ":\n");

  if (eef->has_else) {
    gen_expr_linux_x86_64(gen, eef->elze, TARGET(target.str, true));

    sb_push(&gen->sb, ".l");
    sb_push_i32(&gen->sb, next_id);
    sb_push(&gen->sb, ":\n");
  }

  return target.str;
}

static Str gen_while_linux_x86_64(Generator *gen, ExprWhile *whail, Target target) {
  i32 cond_id = gen->label_count++;
  i32 next_id = gen->label_count++;

  sb_push(&gen->sb, ".l");
  sb_push_i32(&gen->sb, cond_id);
  sb_push(&gen->sb, ":\n");

  Str cond = gen_expr_linux_x86_64(gen, whail->cond, TARGET(target.str, true));

  sb_push(&gen->sb, "\tcmp ");
  sb_push_str(&gen->sb, cond);
  sb_push(&gen->sb, ",0\n");
  sb_push(&gen->sb, "\tje .l");
  sb_push_i32(&gen->sb, next_id);
  sb_push(&gen->sb, "\n");

  gen_expr_linux_x86_64(gen, whail->body, TARGET(target.str, false));

  sb_push(&gen->sb, "\tjmp .l");
  sb_push_i32(&gen->sb, cond_id);
  sb_push(&gen->sb, "\n");

  sb_push(&gen->sb, ".l");
  sb_push_i32(&gen->sb, next_id);
  sb_push(&gen->sb, ":\n");

  return target.str;
}

static Str gen_ret_linux_x86_64(Generator *gen, ExprRet *ret, Target target) {
  Str result = gen_expr_linux_x86_64(gen, ret->result, target);

  if (!str_eq(result, STR_LIT("rax"))) {
    sb_push(&gen->sb, "\tmov rax,");
    sb_push_str(&gen->sb, result);
    sb_push(&gen->sb, "\n");
  }

  if (gen->ctx.func->scope_size != 0) {
    sb_push(&gen->sb, "\tadd rsp,");
    sb_push_i32(&gen->sb, gen->ctx.func->scope_size);
    sb_push(&gen->sb, "\n");
  }

  for (i32 i = gen->ctx.max_regs_used - 3; i >= 0; --i) {
    sb_push(&gen->sb, "\tpop ");
    sb_push_str(&gen->sb, reg_names[i]);
    sb_push(&gen->sb, "\n");
  }

  sb_push(&gen->sb, "\tret\n");

  return target.str;
}

static Str gen_asm_linux_x86_64(Generator *gen, ExprAsm *_asm, Target target) {
  StringBuilder sb = {0};
  AsmNode *node = _asm->nodes;

  while (node) {
    if (node->expr.kind == ExprKindIdent) {
      Str loc = gen_expr_linux_x86_64(gen, node->expr, target);
      sb_push_str(&sb, loc);
    } else if (node->expr.kind == ExprKindLit) {
      if (node->expr.as.lit->kind == LitKindStr)
        sb_push_str(&sb, node->expr.as.lit->lit);
      else if (node->expr.as.lit->kind == LitKindInt)
        sb_push_char(&sb, str_to_i32(node->expr.as.lit->lit));
    }
    node = node->next;
  }

  sb_push_str(&gen->sb, sb_to_str(sb));
  sb_push_char(&gen->sb, '\n');

  return target.str;
}

static Str gen_deref_linux_x86_64 (Generator *gen, ExprDeref *deref, Target target) {
  Str body = gen_expr_linux_x86_64(gen, deref->body, TARGET(target.str, true));
  Str index = gen_expr_linux_x86_64(gen, deref->index, TARGET(reg_names[gen->regs_used++], true));

  sb_push(&gen->sb, "\tadd ");
  sb_push_str(&gen->sb, body);
  sb_push(&gen->sb, ",");
  sb_push_str(&gen->sb, index);
  sb_push(&gen->sb, "\n\tmov ");
  sb_push_str(&gen->sb, body);
  sb_push(&gen->sb, ",");
  sb_push(&gen->sb, "qword [");
  sb_push_str(&gen->sb, body);
  sb_push(&gen->sb, "]\n");

  return target.str;
}

static Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Target target) {
  switch (expr.kind) {
  case ExprKindLit:   return gen_lit_linux_x86_64(gen, expr.as.lit, target);
  case ExprKindBlock: return gen_block_linux_x86_64(gen, expr.as.block, target);
  case ExprKindIdent: return gen_ident_linux_x86_64(gen, expr.as.ident, target);
  case ExprKindCall:  return gen_call_linux_x86_64(gen, expr.as.call, target);
  case ExprKindVar:   return gen_var_linux_x86_64(gen, expr.as.var, target);
  case ExprKindFunc:  return gen_func_linux_x86_64(gen, expr.as.func, target);
  case ExprKindIf:    return gen_if_linux_x86_64(gen, expr.as.eef, target);
  case ExprKindWhile: return gen_while_linux_x86_64(gen, expr.as.whail, target);
  case ExprKindRet:   return gen_ret_linux_x86_64(gen, expr.as.ret, target);
  case ExprKindAsm:   return gen_asm_linux_x86_64(gen, expr.as._asm, target);
  case ExprKindDeref: return gen_deref_linux_x86_64(gen, expr.as.deref, target);
  case ExprKindUse:   return gen_block_linux_x86_64(gen, expr.as.use->content, target);
  }

  ERROR("Unreachable\n");
  exit(1);
}

static void mem_used_count_in_expr(MemUsed *mem_used, Expr expr, bool target_is_return) {
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
    bool preserve_rax_on_rhs_call = target_is_return &&
                                    expr.as.call->args->items[1].kind == ExprKindCall;

    if (expr.as.call->def->is_intrinsic) {
      if (str_eq(expr.as.call->name, STR_LIT("+")) ||
          str_eq(expr.as.call->name, STR_LIT("-")) ||
          str_eq(expr.as.call->name, STR_LIT("*")) ||
          str_eq(expr.as.call->name, STR_LIT("/")) ||
          str_eq(expr.as.call->name, STR_LIT("%"))) {
        if (preserve_rax_on_rhs_call)
          mem_used_use_reg(mem_used);
        mem_used_use_reg(mem_used);

        mem_used_count_in_expr(mem_used, expr.as.call->args->items[0], target_is_return);
        mem_used_count_in_expr(mem_used, expr.as.call->args->items[1], false);

        mem_used->regs_used -= 1 + preserve_rax_on_rhs_call;
      } else if (str_eq(expr.as.call->name, STR_LIT("==")) ||
                 str_eq(expr.as.call->name, STR_LIT("!="))) {
        if (preserve_rax_on_rhs_call)
          mem_used_use_reg(mem_used);
        mem_used_use_reg(mem_used);

        mem_used_count_in_expr(mem_used, expr.as.call->args->items[0], target_is_return);
        mem_used_count_in_expr(mem_used, expr.as.call->args->items[1], false);

        mem_used->regs_used -= 1 + preserve_rax_on_rhs_call;
      }
    } else {
      if (expr.as.call->args->len > mem_used->max_arg_regs_used)
        mem_used->max_arg_regs_used = expr.as.call->args->len;

      i32 args_count = expr.as.call->args->len;
      if (args_count > mem_used->func->arity)
        args_count = mem_used->func->arity;

      for (i32 i = 0; i < expr.as.call->args->len; ++i)
        mem_used_count_in_expr(mem_used, expr.as.call->args->items[i], false);
    }
  } break;

  case ExprKindVar: {
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
    mem_used_count_in_expr(mem_used, expr.as.deref->body, false);
    mem_used_count_in_expr(mem_used, expr.as.deref->index, false);
  } break;

  case ExprKindUse: break;

  default: {
    ERROR("Unreachable\n");
    exit(1);
  }
  }
}

Str gen_linux_x86_64(Metadata meta) {
  Generator gen = {0};
  StringBuilder sb = {0};

  sb_push(&sb, "format ELF64 executable\n");
  sb_push(&sb, "segment readable executable\n");
  sb_push(&sb, "entry _start\n");
  sb_push(&sb, "_start:\n");
  sb_push(&sb, "\tcall main\n");
  sb_push(&sb, "\tmov rdi,rax\n");
  sb_push(&sb, "\tmov rax,60\n");
  sb_push(&sb, "\tsyscall\n");

  for (i32 i = 0; i < meta.funcs.len; ++i) {
    Func func = meta.funcs.items[i];

    if (func.arity > (i32) ARRAY_LEN(arg_reg_names)) {
      ERROR("Exceeded amount of registers for function arguments");
      INFO("TODO: use stack when this happens");
      exit(1);
    }

    def_mangle(func.expr->def);

    Def *arg_def = func.arg_defs;
    for (i32 i = 0; i < func.arity; ++i) {
      arg_def->loc = arg_reg_names[func.arity - i - 1];

      arg_def = arg_def->next;
    }
  }

  for (i32 i = 0; i < meta.funcs.len; ++i) {
    Func func = meta.funcs.items[i];
    MemUsed mem_used = { .func = &func };

    mem_used_count_in_expr(&mem_used, func.expr->body, true);

    gen.sb.len = 0;
    gen.ctx = (FuncCtx) {
      .func = &func,
      .max_regs_used = mem_used.max_regs_used,
      .max_arg_regs_used = mem_used.max_arg_regs_used,
    };

    Def *arg_def = func.arg_defs;
    i32 j = 0;
    while (arg_def) {
      arg_def->loc = arg_reg_names[j];

      arg_def = arg_def->next;
      ++j;
    }

    gen_expr_linux_x86_64(&gen, func.expr->body,
                          TARGET(STR_LIT("rax"), true));

    sb_push_str(&sb, func.expr->def->loc);
    sb_push(&sb, ":\n");

    for (i32 i = 3; i < gen.ctx.max_regs_used; ++i) {
      sb_push(&sb, "\tpush ");
      sb_push_str(&sb, reg_names[i]);
      sb_push(&sb, "\n");
    }

    if (func.scope_size != 0) {
      sb_push(&sb, "\tsub rsp,");
      sb_push_i32(&sb, func.scope_size);
      sb_push(&sb, "\n");
    }

    sb_push_str(&sb, sb_to_str(gen.sb));

    if (func.scope_size != 0) {
      sb_push(&sb, "\tadd rsp,");
      sb_push_i32(&sb, func.scope_size);
      sb_push(&sb, "\n");
    }

    for (i32 i = gen.ctx.max_regs_used - 3; i >= 0; --i) {
      sb_push(&sb, "\tpop ");
      sb_push_str(&sb, reg_names[i]);
      sb_push(&sb, "\n");
    }

    sb_push(&sb, "\tret\n");
  }

  free(gen.sb.buffer);

  if (gen.strings.len > 0)
    sb_push(&sb, "segment readable\n");
  for (i32 i = 0; i < gen.strings.len; ++i) {
    sb_push(&sb, "db");
    sb_push_i32(&sb, i);
    sb_push(&sb, ": db ");
    for (i32 j = 0; j < gen.strings.items[i].len; ++j) {
      if (j != 0)
        sb_push(&sb, ",");
      sb_push_i32(&sb, gen.strings.items[i].ptr[j]);
    }
    sb_push(&sb, ",0\n");
  }

  return sb_to_str(sb);
}
