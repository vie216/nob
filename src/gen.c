#include <stdio.h>
#include <stdlib.h>

#include "gen.h"
#include "log.h"

static Str reg_names[] = { STR("rbx", 3), STR("r12", 3), STR("r13", 3),
                           STR("r14", 3), STR("r15", 3) };
static Str arg_reg_names[] = { STR("rdi", 3), STR("rsi", 3), STR("rdx", 3),
                               STR("rcx", 3), STR("r8", 2),  STR("r9", 2) };

typedef struct {
  bool regs[ARRAY_LEN(reg_names)];
  bool arg_regs[ARRAY_LEN(arg_reg_names)];
} Memory;

static Str mem_reserve(Memory *mem) {
  for (i32 i = 0; i < (i32) ARRAY_LEN(reg_names); ++i) {
    if (!mem->regs[i]) {
      mem->regs[i] = true;
      return reg_names[i];
    }
  }

  ERROR("Exceeded registers count\n");
  INFO("TODO: use stack when this happens\n");
  exit(1);
}

static void mem_free(Memory *mem, Str name) {
  for (i32 i = 0; i < (i32) ARRAY_LEN(reg_names); ++i) {
    if (str_eq(reg_names[i], name)) {
      mem->regs[i] = false;
      return;
    }
  }
}

static Str mem_reserve_arg(Memory *mem) {
  for (i32 i = 0; i < (i32) ARRAY_LEN(arg_reg_names); ++i) {
    if (!mem->arg_regs[i]) {
      mem->arg_regs[i] = true;
      return arg_reg_names[i];
    }
  }

  ERROR("Exceeded arg registers count\n");
  INFO("TODO: use stack when this happens\n");
  exit(1);
}

static void mem_free_args(Memory *mem) {
  for (i32 i = 0; i < (i32) ARRAY_LEN(arg_reg_names); ++i)
    mem->arg_regs[i] = false;
}

typedef struct {
  Str name, data;
} DB;

typedef struct {
  DB *items;
  i32 len, cap;
} DBs;

typedef struct {
  StringBuilder sb;
  Memory        mem;
  DBs           dbs;
  i32           scope_size;
  i32           stack_pointer;
  i32           ifs_count;
} Generator;

static Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Str target, bool strict);

static Str gen_bin_op_linux_x86_64(Generator *gen, ExprBinOp *bin_op, Str target) {
  if (str_eq(bin_op->op, STR("+", 1))
      || str_eq(bin_op->op, STR("-", 1))
      || str_eq(bin_op->op, STR("*", 1))) {
    /* Yes, I didn't come up with a better name */
    bool flag = str_eq(target, STR("rax", 3)) &&
                bin_op->rhs.kind == ExprKindCall;

    Str lhs_loc = target;
    if (flag)
      lhs_loc = mem_reserve(&gen->mem);
    Str lhs = gen_expr_linux_x86_64(gen, bin_op->lhs, lhs_loc, true);

    Str rhs_loc = mem_reserve(&gen->mem);
    Str rhs = gen_expr_linux_x86_64(gen, bin_op->rhs,
                                    rhs_loc, false);

    mem_free(&gen->mem, lhs_loc);
    mem_free(&gen->mem, rhs_loc);

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

static Str gen_lit_linux_x86_64(Generator *gen, ExprLit *lit, Str target, bool strict) {
  if (lit->kind == LitKindInt) {
    if (!strict)
      return lit->lit;

    if (str_eq(lit->lit, STR("0", 1))) {
      sb_push(&gen->sb, "    xor ");
      sb_push_str(&gen->sb, target);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, target);
      sb_push(&gen->sb, "\n");
    } else {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, lit->lit);
      sb_push(&gen->sb, "\n");
    }

    return target;
  } else if (lit->kind == LitKindStr) {
    StringBuilder sb;
    sb_push(&sb, "db_");
    sb_push_i32(&sb, gen->dbs.len);
    DB db = {
      .name = (Str) {
        .ptr = sb.buffer,
        .len = sb.len,
      },
      .data = lit->lit,
    };
    DA_APPEND(gen->dbs, db);

    if (!strict)
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

static Str gen_block_linux_x86_64(Generator *gen, ExprBlock *block, Str target, bool strict) {
  Str mem = mem_reserve(&gen->mem);

  for (i32 i = 0; i + 1 < block->len; ++i)
    gen_expr_linux_x86_64(gen, block->items[i],
                          mem, false);
                          mem_free(&gen->mem, mem);

  if (block->len > 0) {
    Expr last = block->items[block->len - 1];
    return gen_expr_linux_x86_64(gen, last, target, strict);
  }

  return STR("rax", 3);
}

static Str gen_ident_linux_x86_64(Generator *gen, ExprIdent *ident, Str target, bool strict) {
  Str loc = STR("", 0);
  Expr target_expr = ident->target_expr;
  if (target_expr.kind == ExprKindVar)
    loc = target_expr.as.var->loc;
  else if (target_expr.kind == ExprKindFunc)
    loc = target_expr.as.func->name;

  if (!strict)
    return loc;

  sb_push(&gen->sb, "    mov ");
  sb_push_str(&gen->sb, target);
  sb_push(&gen->sb, ", ");
  sb_push_str(&gen->sb, loc);
  sb_push(&gen->sb, "\n");

  return target;
}

static Str gen_call_linux_x86_64(Generator *gen, ExprCall *call, Str target, bool strict) {
  for (i32 i = 0; i < call->args->len; ++i) {
    Expr arg_expr = call->args->items[i];
    Str arg_loc = mem_reserve_arg(&gen->mem);
    gen_expr_linux_x86_64(gen, arg_expr, arg_loc, true);
  }

  Str callee = gen_expr_linux_x86_64(gen, call->func,
                                     STR("rax", 3), false);

  sb_push(&gen->sb, "    call ");
  sb_push_str(&gen->sb, callee);
  sb_push(&gen->sb, "\n");

  if (strict && !str_eq(target, STR("rax", 3))) {
    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target);
    sb_push(&gen->sb, ", rax\n");
  }

  mem_free_args(&gen->mem);
  return strict ? target : STR("rax", 3);
}

static Str gen_var_linux_x86_64(Generator *gen, ExprVar *var, Str target, bool strict) {
  gen->stack_pointer += var->size;

  StringBuilder sb = {0};
  sb_push(&sb, "qword [rsp + ");
  sb_push_i32(&sb, gen->scope_size - gen->stack_pointer);
  sb_push(&sb, "]");
  var->loc = (Str) {
    .ptr = sb.buffer,
    .len = sb.len,
  };

  Str value = gen_expr_linux_x86_64(gen, var->value, target, strict);

  if (!str_eq(value, var->loc)) {
    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, var->loc);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, value);
    sb_push(&gen->sb, "\n");
  }

  return value;
}

static Str gen_func_linux_x86_64(Generator *gen, ExprFunc *func, Str target, bool strict) {
  if (!strict)
    return func->name;

  sb_push(&gen->sb, "    mov ");
  sb_push_str(&gen->sb, target);
  sb_push(&gen->sb, ", ");
  sb_push_str(&gen->sb, func->name);
  sb_push(&gen->sb, "\n");

  return target;
}

static Str gen_if_linux_x86_64(Generator *gen, ExprIf *eef, Str target) {
  Str cond = gen_expr_linux_x86_64(gen, eef->cond,
                                   target, true);
  i32 if_id = gen->ifs_count++;

  sb_push(&gen->sb, "    cmp ");
  sb_push_str(&gen->sb, cond);
  sb_push(&gen->sb, ", 0\n");
  sb_push(&gen->sb, "    je else_");
  sb_push_i32(&gen->sb, if_id);
  sb_push(&gen->sb, "\n");

  gen_expr_linux_x86_64(gen, eef->body,
                        target, true);

  if (eef->has_else) {
    sb_push(&gen->sb, "    jmp next_");
    sb_push_i32(&gen->sb, if_id);
    sb_push(&gen->sb, "\n");
  }
  sb_push(&gen->sb, "  else_");
  sb_push_i32(&gen->sb, if_id);
  sb_push(&gen->sb, ":\n");

  if (eef->has_else)
    gen_expr_linux_x86_64(gen, eef->elze, target, true);

  sb_push(&gen->sb, "  next_");
  sb_push_i32(&gen->sb, if_id);
  sb_push(&gen->sb, ":\n");

  return target;
}

static Str gen_expr_linux_x86_64(Generator *gen, Expr expr, Str target, bool strict) {
  switch (expr.kind) {
  case ExprKindBinOp: return gen_bin_op_linux_x86_64(gen, expr.as.bin_op, target);
  case ExprKindLit:   return gen_lit_linux_x86_64(gen, expr.as.lit, target, strict);
  case ExprKindBlock: return gen_block_linux_x86_64(gen, expr.as.block, target, strict);
  case ExprKindIdent: return gen_ident_linux_x86_64(gen, expr.as.ident, target, strict);
  case ExprKindCall:  return gen_call_linux_x86_64(gen, expr.as.call, target, strict);
  case ExprKindVar:   return gen_var_linux_x86_64(gen, expr.as.var, target, strict);
  case ExprKindFunc:  return gen_func_linux_x86_64(gen, expr.as.func, target, strict);
  case ExprKindIf:    return gen_if_linux_x86_64(gen, expr.as.eef, target);
  }

  ERROR("Unreachable\n");
  exit(1);
}

char *gen_linux_x86_64(Functions funcs) {
  Generator gen = {0};

  sb_push(&gen.sb, "format ELF64 executable\n");
  sb_push(&gen.sb, "entry _start\n");
  sb_push(&gen.sb, "segment readable executable\n");
  sb_push(&gen.sb, "_start:\n");
  sb_push(&gen.sb, "    call main\n");
  sb_push(&gen.sb, "    mov rdi, rax\n");
  sb_push(&gen.sb, "    mov rax, 60\n");
  sb_push(&gen.sb, "    syscall\n");

  for (i32 i = 0; i < funcs.len; ++i) {
    sb_push_str(&gen.sb, funcs.items[i]->name);
    sb_push(&gen.sb, ":\n");

    sb_push(&gen.sb, "    push rbx\n");
    sb_push(&gen.sb, "    push r12\n");
    sb_push(&gen.sb, "    push r13\n");
    sb_push(&gen.sb, "    push r14\n");
    sb_push(&gen.sb, "    push r15\n");

    if (funcs.items[i]->scope_size != 0) {
      sb_push(&gen.sb, "    sub rsp, ");
      sb_push_i32(&gen.sb, funcs.items[i]->scope_size);
      sb_push(&gen.sb, "\n");
    }

    gen.scope_size = funcs.items[i]->scope_size;
    gen_expr_linux_x86_64(&gen, funcs.items[i]->body,
                          STR("rax", 3), true);

    if (funcs.items[i]->scope_size != 0) {
      sb_push(&gen.sb, "    add rsp, ");
      sb_push_i32(&gen.sb, funcs.items[i]->scope_size);
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

  if (gen.dbs.len > 0)
    sb_push(&gen.sb, "segment readable\n");
  for (i32 i = 0; i < gen.dbs.len; ++i) {
    sb_push_str(&gen.sb, gen.dbs.items[i].name);
    sb_push(&gen.sb, ": db ");
    sb_push_str(&gen.sb, gen.dbs.items[i].data);
    sb_push(&gen.sb, ", 0\n");
  }

  return gen.sb.buffer;
}
