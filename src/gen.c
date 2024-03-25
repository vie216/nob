#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen.h"
#include "log.h"

typedef struct {
  Str *items;
  i32 len, cap;
} Strings;

typedef struct {
  StringBuilder sb;
  Strings       strings;
  i32           regs_used;
  i32           scope_size;
  i32           stack_pointer;
  i32           ifs_count;
} Generator;

typedef struct {
  Str  str;
  bool strict;
} Target;

#define TARGET(loc, strict) ((Target) { loc, strict })

static Str gen_reserve_reg(Generator *gen) {
  static Str reg_names[] = { STR("rbx", 3), STR("r12", 3), STR("r13", 3),
                             STR("r14", 3), STR("r15", 3) };

  if (gen->regs_used >= (i32) ARRAY_LEN(reg_names)) {
    ERROR("Exceeded amount of available registers");
    exit(1);
  }

  return reg_names[gen->regs_used++];
}

static Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Target target);

static Str gen_bin_op_linux_x86_64(Generator *gen, ExprBinOp *bin_op, Target target) {
  if (str_eq(bin_op->op, STR("+", 1))
      || str_eq(bin_op->op, STR("-", 1))
      || str_eq(bin_op->op, STR("*", 1))) {
    /* Yes, I didn't come up with a better name */
    bool flag = str_eq(target.str, STR("rax", 3)) &&
                bin_op->rhs.kind == ExprKindCall;

    Str lhs_loc = target.str;
    if (flag)
      lhs_loc = gen_reserve_reg(gen);
    Str lhs = gen_expr_linux_x86_64(gen, bin_op->lhs, TARGET(lhs_loc, true));

    Str rhs_loc = gen_reserve_reg(gen);
    Str rhs = gen_expr_linux_x86_64(gen, bin_op->rhs, TARGET(rhs_loc, false));

    gen->regs_used -= 1 + flag;

    if (str_eq(bin_op->op, STR("+", 1)))
      sb_push(&gen->sb, "    add ");
    else if (str_eq(bin_op->op, STR("-", 1)))
      sb_push(&gen->sb, "    sub ");
    else
      sb_push(&gen->sb, "    imul ");
    sb_push_str(&gen->sb, lhs);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, rhs);
    sb_push(&gen->sb, "\n");

    if (flag) {
      sb_push(&gen->sb, "    mov rax, ");
      sb_push_str(&gen->sb, lhs_loc);
      sb_push(&gen->sb, "\n");
    }

    return lhs;
  }

  ERROR("Unknown operator: `");
  str_fprint(stderr, bin_op->op);
  fprintf(stderr, "`\n");
  exit(1);
}

static Str gen_lit_linux_x86_64(Generator *gen, ExprLit *lit, Target target) {
  if (lit->kind == LitKindInt) {
    if (!target.strict)
      return lit->lit;

    if (str_eq(lit->lit, STR("0", 1))) {
      sb_push(&gen->sb, "    xor ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, "\n");
    } else {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, lit->lit);
      sb_push(&gen->sb, "\n");
    }

    return target.str;
  } else if (lit->kind == LitKindStr) {
    DA_APPEND(gen->strings, lit->lit);

    if (!target.strict) {
      char *name = malloc(lit->lit.len + 3);
      name[0] = 'd';
      name[1] = 'b';
      name[2] = '_';
      memmove(name + 3, lit->lit.ptr, lit->lit.len);
      return (Str) {
        .ptr = name,
        .len = lit->lit.len + 3,
      };
    }

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ", db_");
    sb_push_str(&gen->sb, lit->lit);
    sb_push(&gen->sb, "\n");

    return target.str;
  }

  ERROR("Unreachable\n");
  exit(1);
}

static Str gen_block_linux_x86_64(Generator *gen, ExprBlock *block, Target target) {
  Str mem = gen_reserve_reg(gen);

  for (i32 i = 0; i + 1 < block->len; ++i)
    gen_expr_linux_x86_64(gen, block->items[i], TARGET(mem, false));

  gen->regs_used--;

  if (block->len > 0) {
    Expr last = block->items[block->len - 1];
    return gen_expr_linux_x86_64(gen, last, target);
  }

  return STR("rax", 3);
}

static Str gen_ident_linux_x86_64(Generator *gen, ExprIdent *ident, Target target) {
  if (!target.strict)
    return ident->def->loc;

  sb_push(&gen->sb, "    mov ");
  sb_push_str(&gen->sb, target.str);
  sb_push(&gen->sb, ", ");
  sb_push_str(&gen->sb, ident->def->loc);
  sb_push(&gen->sb, "\n");

  return target.str;
}

static Str gen_call_linux_x86_64(Generator *gen, ExprCall *call, Target target) {
  static Str arg_reg_names[] = { STR("rdi", 3), STR("rsi", 3), STR("rdx", 3),
                                 STR("rcx", 3), STR("r8", 2), STR("r9", 2) };

  if (call->args->len > (i32) ARRAY_LEN(arg_reg_names)) {
    ERROR("Exceeded amount of registers for function arguments\n");
    INFO("TODO: use stack when this happens\n");
    exit(1);
  }

  for (i32 i = 0; i < call->args->len; ++i)
    gen_expr_linux_x86_64(gen, call->args->items[i], TARGET(arg_reg_names[i], true));

  Str callee = gen_expr_linux_x86_64(gen, call->func, TARGET(STR("rax", 3), false));

  sb_push(&gen->sb, "    call ");
  sb_push_str(&gen->sb, callee);
  sb_push(&gen->sb, "\n");

  if (target.strict && !str_eq(target.str, STR("rax", 3))) {
    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ", rax\n");
  }

  return target.strict ? target.str : STR("rax", 3);
}

static Str gen_var_linux_x86_64(Generator *gen, ExprVar *var, Target target) {
  gen->stack_pointer += var->def->size;

  StringBuilder sb = {0};
  sb_push(&sb, "qword [rsp + ");
  sb_push_i32(&sb, gen->scope_size - gen->stack_pointer);
  sb_push(&sb, "]");
  var->def->loc = (Str) {
    .ptr = sb.buffer,
    .len = sb.len,
  };

  Str value = gen_expr_linux_x86_64(gen, var->value, target);

  if (!str_eq(value, var->def->loc)) {
    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, var->def->loc);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, value);
    sb_push(&gen->sb, "\n");
  }

  return value;
}

static Str gen_func_linux_x86_64(Generator *gen, ExprFunc *func, Target target) {
  if (!target.strict)
    return func->def->loc;

  sb_push(&gen->sb, "    mov ");
  sb_push_str(&gen->sb, target.str);
  sb_push(&gen->sb, ", ");
  sb_push_str(&gen->sb, func->def->loc);
  sb_push(&gen->sb, "\n");

  return target.str;
}

static Str gen_if_linux_x86_64(Generator *gen, ExprIf *eef, Target target) {
  Str cond = gen_expr_linux_x86_64(gen, eef->cond, TARGET(target.str, true));
  i32 if_id = gen->ifs_count++;

  sb_push(&gen->sb, "    cmp ");
  sb_push_str(&gen->sb, cond);
  sb_push(&gen->sb, ", 0\n");
  sb_push(&gen->sb, "    je else_");
  sb_push_i32(&gen->sb, if_id);
  sb_push(&gen->sb, "\n");

  gen_expr_linux_x86_64(gen, eef->body, TARGET(target.str, true));

  if (eef->has_else) {
    sb_push(&gen->sb, "    jmp next_");
    sb_push_i32(&gen->sb, if_id);
    sb_push(&gen->sb, "\n");
  }
  sb_push(&gen->sb, "  else_");
  sb_push_i32(&gen->sb, if_id);
  sb_push(&gen->sb, ":\n");

  if (eef->has_else)
    gen_expr_linux_x86_64(gen, eef->elze, TARGET(target.str, true));

  sb_push(&gen->sb, "  next_");
  sb_push_i32(&gen->sb, if_id);
  sb_push(&gen->sb, ":\n");

  return target.str;
}

static Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Target target) {
  switch (expr.kind) {
  case ExprKindBinOp: return gen_bin_op_linux_x86_64(gen, expr.as.bin_op, target);
  case ExprKindLit:   return gen_lit_linux_x86_64(gen, expr.as.lit, target);
  case ExprKindBlock: return gen_block_linux_x86_64(gen, expr.as.block, target);
  case ExprKindIdent: return gen_ident_linux_x86_64(gen, expr.as.ident, target);
  case ExprKindCall:  return gen_call_linux_x86_64(gen, expr.as.call, target);
  case ExprKindVar:   return gen_var_linux_x86_64(gen, expr.as.var, target);
  case ExprKindFunc:  return gen_func_linux_x86_64(gen, expr.as.func, target);
  case ExprKindIf:    return gen_if_linux_x86_64(gen, expr.as.eef, target);
  }

  ERROR("Unreachable\n");
  exit(1);
}

char *gen_linux_x86_64(Metadata meta) {
  Generator gen = {0};

  sb_push(&gen.sb, "format ELF64 executable\n");
  sb_push(&gen.sb, "entry _start\n");
  sb_push(&gen.sb, "segment readable executable\n");
  sb_push(&gen.sb, "_start:\n");
  sb_push(&gen.sb, "    call main\n");
  sb_push(&gen.sb, "    mov rdi, rax\n");
  sb_push(&gen.sb, "    mov rax, 60\n");
  sb_push(&gen.sb, "    syscall\n");

  for (i32 i = 0; i < meta.funcs.len; ++i) {
    Func func = meta.funcs.items[i];

    sb_push_str(&gen.sb, func.expr->name);
    sb_push(&gen.sb, ":\n");

    sb_push(&gen.sb, "    push rbx\n");
    sb_push(&gen.sb, "    push r12\n");
    sb_push(&gen.sb, "    push r13\n");
    sb_push(&gen.sb, "    push r14\n");
    sb_push(&gen.sb, "    push r15\n");

    if (func.scope_size != 0) {
      sb_push(&gen.sb, "    sub rsp, ");
      sb_push_i32(&gen.sb, func.scope_size);
      sb_push(&gen.sb, "\n");
    }

    gen.scope_size = func.scope_size;
    gen_expr_linux_x86_64(&gen, func.expr->body,
                          TARGET(STR("rax", 3), true));

    if (func.scope_size != 0) {
      sb_push(&gen.sb, "    add rsp, ");
      sb_push_i32(&gen.sb, func.scope_size);
      sb_push(&gen.sb, "\n");
    }

    sb_push(&gen.sb, "    pop r15\n");
    sb_push(&gen.sb, "    pop r14\n");
    sb_push(&gen.sb, "    pop r13\n");
    sb_push(&gen.sb, "    pop r12\n");
    sb_push(&gen.sb, "    pop rbx\n");

    sb_push(&gen.sb, "    ret\n");

    gen.scope_size = 0;
  }

  if (gen.strings.len > 0)
    sb_push(&gen.sb, "segment readable\n");
  for (i32 i = 0; i < gen.strings.len; ++i) {
    sb_push(&gen.sb, "db_");
    sb_push_i32(&gen.sb, i);
    sb_push(&gen.sb, ": db ");
    sb_push_str(&gen.sb, gen.strings.items[i]);
    sb_push(&gen.sb, ", 0\n");
  }

  return gen.sb.buffer;
}
