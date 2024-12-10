#include "type.h"

typedef struct {
  Func *func;
  i32   scope_size;
  i32   max_args_count_in_subcall;
  i32   max_regs_used;
  i32   regs_used;
  i32   max_arg_regs_used;
  i32   arg_regs_used;
} MemUsed;

void mem_used_count_in_expr(MemUsed *mem_used, Expr expr, bool target_is_return);
