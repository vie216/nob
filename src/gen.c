#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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

Str mem_reserve(Memory *mem) {
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

void mem_free(Memory *mem, Str name) {
  for (i32 i = 0; i < (i32) ARRAY_LEN(reg_names); ++i) {
    if (str_eq(reg_names[i], name)) {
      mem->regs[i] = false;
      return;
    }
  }
}

Str mem_reserve_arg(Memory *mem) {
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

void mem_free_args(Memory *mem) {
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
  Str name, loc;
} Var;

typedef struct {
  Var *items;
  i32  len, cap;
} Vars;

typedef struct {
  ExprFun **items;
  i32       len, cap;
} Funs;

typedef struct {
  StringBuilder sb;
  Memory        mem;
  DBs           dbs;
  Vars          vars;
  Funs          funs;
  i32           block_size;
} Generator;

typedef enum {
  LocKindAny = 0,
  LocKindRegOrMem,
  LocKindCertain,
} LocKind;

typedef struct {
  LocKind    kind;
  Str        str;
} Loc;

#define LOC(kind, str) ((Loc) { kind, str })
#define NUM_LEN(num) (num == 0 ? 1 : (int)(log10(num)+1))

Loc gen_expr_linux_x86_64(Generator *gen, Expr expr, Loc target) {
  switch (expr.kind) {
  case ExprKindBlock: ;
    Loc res = LOC(LocKindAny, {0});
    i32 len = gen->vars.len;
    i32 size = gen->block_size;

    for (i32 i = 0; i + 1 < expr.as.block->len; ++i)
      mem_free(&gen->mem, gen_expr_linux_x86_64(gen, expr.as.block->items[i], LOC(LocKindAny, {0})).str);

    if (expr.as.block->len > 0)
      res = gen_expr_linux_x86_64(gen, expr.as.block->items[expr.as.block->len - 1], target);

    gen->vars.len = len;
    gen->block_size = size;
    return res;
  case ExprKindLit:
    if (expr.as.lit->kind == LitKindInt) {
      if (target.kind == LocKindAny)
        return LOC(LocKindAny, expr.as.lit->lit);
      if (target.kind == LocKindRegOrMem)
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
      DB db;
      db.name.ptr = malloc(NUM_LEN(gen->dbs.len));
      db.name.len = sprintf(db.name.ptr, "db_%d", gen->dbs.len);
      db.data = expr.as.lit->lit;
      DA_APPEND(gen->dbs, db);

      if (target.kind == LocKindAny)
        return LOC(LocKindAny, db.name);
      if (target.kind == LocKindRegOrMem)
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
  case ExprKindBinOp:
    if (str_eq(expr.as.bin_op->op, STR("+", 1))
        || str_eq(expr.as.bin_op->op, STR("-", 1))
        || str_eq(expr.as.bin_op->op, STR("*", 1))) {
      if (target.kind == LocKindAny)
        target.kind = LocKindRegOrMem;

      Loc lhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->lhs, target);
      Loc rhs = gen_expr_linux_x86_64(gen, expr.as.bin_op->rhs, LOC(LocKindAny, {0}));
      if (rhs.kind == LocKindRegOrMem)
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
    } else {
      ERROR("Unknown operator: `");
      str_print(expr.as.bin_op->op);
      printf("`\n");
      exit(1);
    }
  case ExprKindIdent:
    for (i32 i = gen->vars.len - 1; i >= 0; --i) {
      Var var = gen->vars.items[i];
      if (str_eq(var.name, expr.as.ident->ident)) {
        if (target.kind == LocKindAny)
          return LOC(LocKindAny, var.loc);

        if (target.kind == LocKindRegOrMem)
          target.str = mem_reserve(&gen->mem);

        sb_push(&gen->sb, "    mov ");
        sb_push_str(&gen->sb, target.str);
        sb_push(&gen->sb, ", ");
        sb_push_str(&gen->sb, var.loc);
        sb_push(&gen->sb, "\n");

        return target;
      }
    }

    ERROR("`");
    str_print(expr.as.ident->ident);
    puts("` undeclared");
    exit(1);
  case ExprKindVar:
    gen->block_size += 8;

    Str loc;
    loc.len = NUM_LEN(gen->block_size) + 14;
    loc.ptr = malloc(loc.len);
    sprintf(loc.ptr, "qword [rbp - %d]", gen->block_size);

    if (target.kind != LocKindCertain) {
      if (expr.as.var->value.kind == ExprKindLit)
        target = LOC(LocKindCertain, loc);
      else
        target.kind = LocKindRegOrMem;
    }

    Loc value = gen_expr_linux_x86_64(gen, expr.as.var->value, target);
    if (value.kind == LocKindRegOrMem)
      mem_free(&gen->mem, value.str);

    if (target.kind != LocKindCertain || !str_eq(target.str, loc)) {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, loc);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, value.str);
      sb_push(&gen->sb, "\n");
    }

    DA_APPEND(gen->vars, ((Var) { expr.as.var->name, loc, }));

    return target;
  case ExprKindCall:
    for (i32 i = 0; i < expr.as.call->args->len; ++i) {
      Str arg = mem_reserve_arg(&gen->mem);
      gen_expr_linux_x86_64(gen, expr.as.call->args->items[i],
                            LOC(LocKindCertain, arg));
    }

    sb_push(&gen->sb, "    call ");
    sb_push_str(&gen->sb, expr.as.call->name);
    sb_push(&gen->sb, "\n");

    if (target.kind == LocKindCertain) {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", rax\n");
    }

    mem_free_args(&gen->mem);
    return target;
  case ExprKindFun:
    DA_APPEND(gen->funs, expr.as.fun);

    if (target.kind == LocKindAny)
      return LOC(LocKindAny, expr.as.fun->name);
    if (target.kind == LocKindRegOrMem)
      target.str = mem_reserve(&gen->mem);

    sb_push(&gen->sb, "    mov ");
    sb_push_str(&gen->sb, target.str);
    sb_push(&gen->sb, ", ");
    sb_push_str(&gen->sb, expr.as.fun->name);
    sb_push(&gen->sb, "\n");

    return target;
  default:
    ERROR("Not implemented\n");
    exit(1);
  }
}

char *gen_linux_x86_64(Expr expr) {
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

  gen_expr_linux_x86_64(&gen, expr, LOC(LocKindAny, {0}));

  bool found_main = false;
  for (i32 i = 0; i < gen.funs.len; ++i) {
    if (str_eq(gen.funs.items[i]->name, STR("main", 4)))
      found_main = true;

    sb_push_str(&gen.sb, gen.funs.items[i]->name);
    sb_push(&gen.sb, ":\n");
    sb_push(&gen.sb, "    push rbp\n");
    sb_push(&gen.sb, "    mov rbp, rsp\n");
    gen_expr_linux_x86_64(&gen, gen.funs.items[i]->body,
                          LOC(LocKindCertain, STR("rax", 3)));
    sb_push(&gen.sb, "    pop rbp\n");
    sb_push(&gen.sb, "    ret\n");
  }

  if (!found_main) {
    ERROR("`main` function is required\n");
    exit(1);
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
