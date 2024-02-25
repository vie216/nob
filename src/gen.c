#include <stdio.h>
#include <stdlib.h>

#include "gen.h"
#include "log.h"

static Str reg_names[] = { STR("rbx", 3), STR("r10", 3), STR("r11", 3), STR("r12", 3),
                           STR("r13", 3), STR("r14", 3), STR("r15", 3) };
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
} Generator;

typedef enum {
  LocKindAny = 0,
  LocKindReg,
  LocKindRegOrMem,
  LocKindCertain,
} LocKind;

typedef struct {
  LocKind    kind;
  Str        str;
} Loc;

#define LOC(kind, str) ((Loc) { kind, str })

static Loc gen_expr_linux_x86_64(Generator *gen, Expr expr, Loc target) {
  switch (expr.kind) {
  case ExprKindBlock: {
    for (i32 i = 0; i + 1 < expr.as.block->len; ++i) {
      Loc loc = gen_expr_linux_x86_64(gen, expr.as.block->items[i],
                                      LOC(LocKindAny, {0}));
      mem_free(&gen->mem, loc.str);
    }

    if (expr.as.block->len > 0)
      return gen_expr_linux_x86_64(gen, expr.as.block->items[expr.as.block->len - 1], target);
    return LOC(LocKindAny, {0});
  }

  case ExprKindLit: {
    if (expr.as.lit->kind == LitKindInt) {
      if (target.kind == LocKindAny)
        return LOC(LocKindAny, expr.as.lit->lit);
      if (target.kind == LocKindRegOrMem)
        target.kind = LocKindReg;
      if (target.kind == LocKindReg)
        target.str = mem_reserve(&gen->mem);

      if (str_eq(expr.as.lit->lit, STR("0", 1))) {
        sb_push(&gen->sb, "    xor ");
        sb_push_str(&gen->sb, target.str);
        sb_push(&gen->sb, ", ");
        sb_push_str(&gen->sb, target.str);
        sb_push(&gen->sb, "\n");
      } else {
        sb_push(&gen->sb, "    mov ");
        sb_push_str(&gen->sb, target.str);
        sb_push(&gen->sb, ", ");
        sb_push_str(&gen->sb, expr.as.lit->lit);
        sb_push(&gen->sb, "\n");
      }

      return target;
    } else if (expr.as.lit->kind == LitKindStr) {
      StringBuilder sb;
      sb_push(&sb, "db_");
      sb_push_i32(&sb, gen->dbs.len);
      DB db = {
        .name = (Str) {
          .ptr = sb.buffer,
          .len = sb.len,
        },
        .data = expr.as.lit->lit,
      };
      DA_APPEND(gen->dbs, db);

      if (target.kind == LocKindAny)
        return LOC(LocKindAny, db.name);
      if (target.kind == LocKindRegOrMem)
        target.kind = LocKindReg;
      if (target.kind == LocKindReg)
        target.str = mem_reserve(&gen->mem);

      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, db.name);
      sb_push(&gen->sb, "\n");

      return target;
    }

    ERROR("Unreachable\n");
    exit(1);
  }

  case ExprKindBinOp: {
    if (str_eq(expr.as.bin_op->op, STR("+", 1))
        || str_eq(expr.as.bin_op->op, STR("-", 1))
        || str_eq(expr.as.bin_op->op, STR("*", 1))) {
      if (target.kind == LocKindAny)
        target.kind = LocKindRegOrMem;

      Loc lhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->lhs, target);
      Loc rhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->rhs, LOC(LocKindAny, {0}));
      if (rhs.kind == LocKindRegOrMem || rhs.kind == LocKindReg)
        mem_free(&gen->mem, rhs.str);

      if (str_eq(expr.as.bin_op->op, STR("+", 1)))
        sb_push(&gen->sb, "    add ");
      else if (str_eq(expr.as.bin_op->op, STR("-", 1)))
        sb_push(&gen->sb, "    sub ");
      else
        sb_push(&gen->sb, "    imul ");
      sb_push_str(&gen->sb, lhs.str);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, rhs.str);
      sb_push(&gen->sb, "\n");

      return lhs;
    }

    ERROR("Unknown operator: `");
    str_print(expr.as.bin_op->op);
    printf("`\n");
    exit(1);
  }

  case ExprKindIdent: {
    Str loc = STR("", 0);
    Expr target_expr = expr.as.ident->target_expr;
    if (target_expr.kind == ExprKindVar)
      loc = target_expr.as.var->loc;
    else if (target_expr.kind == ExprKindFunc)
      loc = target_expr.as.func->name;

    if (target.kind == LocKindAny || target.kind == LocKindRegOrMem)
      return LOC(target.kind, loc);
    else if (target.kind == LocKindReg)
      target.str = mem_reserve(&gen->mem);

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, loc);
    sb_push(&gen->sb, "\n");

    return target;
  }

  case ExprKindVar: {
    gen->scope_size += expr.as.var->size;

    StringBuilder sb = {0};
    sb_push(&sb, "qword [rbp - ");
    sb_push_i32(&sb, gen->scope_size);
    sb_push(&sb, "]");
    expr.as.var->loc = (Str) {
      .ptr = sb.buffer,
      .len = sb.len,
    };

    if (target.kind != LocKindCertain) {
      if (expr.as.var->value.kind == ExprKindLit)
        target = LOC(LocKindCertain, expr.as.var->loc);
      else
        target.kind = LocKindReg;
    }

    Loc value = gen_expr_linux_x86_64(gen, expr.as.var->value, target);
    if (value.kind == LocKindReg)
      mem_free(&gen->mem, value.str);

    if (!str_eq(value.str, expr.as.var->loc)) {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, expr.as.var->loc);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, value.str);
      sb_push(&gen->sb, "\n");
    }

    return value;
  }

  case ExprKindCall: {
    for (i32 i = 0; i < expr.as.call->args->len; ++i) {
      Str arg_loc = mem_reserve_arg(&gen->mem);
      gen_expr_linux_x86_64(gen, expr.as.call->args->items[i],
                            LOC(LocKindCertain, arg_loc));
    }

    Loc func_loc = gen_expr_linux_x86_64(gen, expr.as.call->func,
                                         LOC(LocKindAny, {0}));
    sb_push(&gen->sb, "    call ");
    sb_push_str(&gen->sb, func_loc.str);
    sb_push(&gen->sb, "\n");

    if (target.kind == LocKindCertain && !str_eq(target.str, STR("rax", 3))) {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", rax\n");
    }

    mem_free_args(&gen->mem);
    return target;
  }

  case ExprKindFunc: {
    expr.as.func->loc = expr.as.func->name;

    if (target.kind == LocKindAny)
      return LOC(LocKindAny, expr.as.func->name);
    if (target.kind == LocKindRegOrMem || target.kind == LocKindReg)
      target.str = mem_reserve(&gen->mem);

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, expr.as.func->name);
    sb_push(&gen->sb, "\n");

    return target;
  }
  }

  ERROR("Not implemented\n");
  exit(1);
}

char *gen_linux_x86_64(Functions funcs) {
  Generator gen = {0};

  sb_push(&gen.sb, "format ELF64 executable\n");
  sb_push(&gen.sb, "entry _start\n");
  sb_push(&gen.sb, "segment readable executable\n");
  sb_push(&gen.sb, "_start:\n");
  sb_push(&gen.sb, "    mov rbp, rsp\n");
  sb_push(&gen.sb, "    call main\n");
  sb_push(&gen.sb, "    mov rdi, rax\n");
  sb_push(&gen.sb, "    mov rax, 60\n");
  sb_push(&gen.sb, "    syscall\n");

  for (i32 i = 0; i < funcs.len; ++i) {
    sb_push_str(&gen.sb, funcs.items[i]->name);
    sb_push(&gen.sb, ":\n");
    sb_push(&gen.sb, "    push rbp\n");
    sb_push(&gen.sb, "    mov rbp, rsp\n");

    if (funcs.items[i]->scope_size != 0) {
      sb_push(&gen.sb, "    sub rsp, ");
      sb_push_i32(&gen.sb, funcs.items[i]->scope_size);
      sb_push(&gen.sb, "\n");
    }

    gen_expr_linux_x86_64(&gen, funcs.items[i]->body,
                          LOC(LocKindCertain, STR("rax", 3)));

    if (funcs.items[i]->scope_size != 0) {
      sb_push(&gen.sb, "    add rsp, ");
      sb_push_i32(&gen.sb, funcs.items[i]->scope_size);
      sb_push(&gen.sb, "\n");
    }

    sb_push(&gen.sb, "    pop rbp\n");
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
