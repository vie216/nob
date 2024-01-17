#ifndef GEN_H
#define GEN_H

#include "parser.h"
#include "str.h"

Str gen_expr_linux_x86_64(StringBuilder *sb, Expr expr, Str target, bool force);
char *gen_linux_x86_64(Expr expr);

#endif // GEN_H
