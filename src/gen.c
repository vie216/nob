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

typedef enum {
  SymbolKindVar = 0,
} SymbolKind;

typedef struct {
  Str name, loc;
} SymbolVar;

typedef union {
  SymbolVar var;
} SymbolAs;

typedef struct {
  SymbolKind kind;
  SymbolAs   as;
} Symbol;

typedef struct {
  Symbol *items;
  i32     len, cap;
} Symbols;

typedef struct {
  StringBuilder sb;
  Memory        mem;
  DBs           dbs;
  Symbols       syms;
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
  case ExprKindBlock:
    Loc res = LOC(LocKindAny, {0});
    i32 len = gen->syms.len;
    i32 size = gen->block_size;

    for (i32 i = 0; i + 1 < expr.as.block->len; ++i)
      mem_free(&gen->mem, gen_expr_linux_x86_64(gen, expr.as.block->items[i], LOC(LocKindAny, {0})).str);

    if (expr.as.block->len > 0)
      res = gen_expr_linux_x86_64(gen, expr.as.block->items[expr.as.block->len - 1], target);

    gen->syms.len = len;
    gen->block_size = size;
    return res;
  case ExprKindIntLit:
    if (target.kind == LocKindAny)
      return LOC(LocKindAny, expr.as.int_lit->lit);

    if (target.kind == LocKindRegOrMem)
      target.str = mem_reserve(&gen->mem);

    if (str_eq(expr.as.int_lit->lit, STR("0", 1))) {
      sb_push(&gen->sb, "    xor ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, "\n");
    } else {
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, target.str);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, expr.as.int_lit->lit);
      sb_push(&gen->sb, "\n");
    }

    return target;
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
  case ExprKindStrLit:
    DB db;
    db.name.ptr = malloc(NUM_LEN(gen->dbs.len));
    db.name.len = sprintf(db.name.ptr, "db_%d", gen->dbs.len);
    db.data = expr.as.str_lit->lit;
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
  case ExprKindIdent:
    for (i32 i = gen->syms.len - 1; i >= 0; --i) {
      Symbol sym = gen->syms.items[i];
      if (sym.kind == SymbolKindVar
          && str_eq(sym.as.var.name, expr.as.ident->ident)) {
        if (target.kind == LocKindAny)
          return LOC(LocKindAny, sym.as.var.loc);

        if (target.kind == LocKindRegOrMem)
          target.str = mem_reserve(&gen->mem);

        sb_push(&gen->sb, "    mov ");
        sb_push_str(&gen->sb, target.str);
        sb_push(&gen->sb, ", ");
        sb_push_str(&gen->sb, sym.as.var.loc);
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

    if (target.kind == LocKindCertain) {
      Loc value = gen_expr_linux_x86_64(gen, expr.as.var->value, target);
      sb_push(&gen->sb, "    mov ");
      sb_push_str(&gen->sb, loc);
      sb_push(&gen->sb, ", ");
      sb_push_str(&gen->sb, value.str);
      sb_push(&gen->sb, "\n");
    } else {
      target = LOC(LocKindCertain, loc);
      gen_expr_linux_x86_64(gen, expr.as.var->value, target);
    }

    DA_APPEND(gen->syms, ((Symbol) {
          SymbolKindVar,
          { .var = {
              expr.as.var->name,
              loc,
            } },
        }));

    return target;
  case ExprKindCall:
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

  gen_expr_linux_x86_64(&gen, expr, LOC(LocKindCertain, STR("rdi", 3)));

  sb_push(&gen.sb, "    mov rax, 60\n");
  sb_push(&gen.sb, "    syscall\n");

  if (gen.dbs.len > 0)
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
