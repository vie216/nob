#include <string.h>

#include "defs.h"
#include "str.h"
#include "log.h"
#include "parser.h"
#include "type.h"
#include "gen-linux-x86_64.h"
#include "io.h"

char *write_asm(char *path, Str _asm) {
  i32 path_len = strlen(path);
  char *new_path = malloc(path_len + 5);
  for (i32 i = 0; i < path_len; ++i)
    new_path[i] = path[i];
  new_path[path_len + 0] = '.';
  new_path[path_len + 1] = 'a';
  new_path[path_len + 2] = 's';
  new_path[path_len + 3] = 'm';
  new_path[path_len + 4] = '\0';

  FILE *file = fopen(new_path, "w");
  fprintf(file, "%.*s", _asm.len, _asm.ptr);
  fclose(file);

  return new_path;
}

void assemble(char *asm_path) {
  i32 len = strlen(asm_path);
  char *cmd = malloc(len + 5);
  for (i32 i = 0; i < 5; ++i)
    cmd[i] = "fasm "[i];
  for (i32 i = 0; i < len; ++i)
    cmd[i + 5] = asm_path[i];
  cmd[len + 5] = '\0';
  if (system(cmd))
    exit(1);
}

#define DEF_INT_BINOP(op_name)                                             \
  do {                                                                     \
    func = aalloc(sizeof(TypeFunc));                                       \
    int_arg = aalloc(sizeof(TypeInt));                                     \
    int_arg->signedd = true;                                               \
    func->name = STR_LIT(op_name);                                         \
    func->result_type = (Type) { TypeKindInt, { .eent = int_arg }, size }; \
    func->arg_defs = arg_defs;                                             \
    func->arity = 2;                                                       \
    bin_op_type = (Type) { TypeKindFunc, { .func = func }, size };         \
  } while (0)

Def *intrinsic_defs(void) {
  Def *defs = NULL;
  Def *arg_defs = NULL;
  Str size = STR_LIT("8");

  arg_defs = aalloc(sizeof(Def));
  arg_defs->next = aalloc(sizeof(Def));

  TypeInt *int_arg = aalloc(sizeof(TypeInt));
  int_arg->signedd = true;
  arg_defs->type = (Type) { TypeKindInt, { .eent = int_arg }, size };

  int_arg = aalloc(sizeof(TypeInt));
  int_arg->signedd = true;
  arg_defs->next->type = (Type) { TypeKindInt, { .eent = int_arg }, size };

  TypeFunc *func = aalloc(sizeof(TypeFunc));
  int_arg = aalloc(sizeof(TypeInt));
  int_arg->signedd = true;
  func->result_type = (Type) { TypeKindInt, { .eent = int_arg }, size };
  func->arg_defs = arg_defs;
  int_arg = aalloc(sizeof(TypeInt));
  int_arg->signedd = true;
  arg_defs->next->type = (Type) { TypeKindInt, { .eent = int_arg }, size };

  Type bin_op_type;

  DEF_INT_BINOP("=");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("=");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("+");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("+");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("-");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("-");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("*");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("*");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("/");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("/");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("%");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("%");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("==");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("==");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("!=");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("!=");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP(">");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT(">");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("<");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("<");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP(">=");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT(">=");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  DEF_INT_BINOP("<=");
  LL_APPEND(defs, Def);
  defs->name = STR_LIT("<=");
  defs->type = bin_op_type;
  defs->is_intrinsic = true;

  return defs;
}

int main(int argc, char **argv) {
  char *input_path, *output_path;
  parse_args(argc, argv, &input_path, &output_path);
  Str source_code = read_file(input_path);

  INFO("Parsing\n");
  Expr program = parse_program(source_code, input_path);
  INFO("Type checking\n");
  Metadata meta = type_check(program, intrinsic_defs());
  INFO("Compiling\n");
  Str _asm = gen_linux_x86_64(meta);
  INFO("Assembling\n");
  char *asm_path = write_asm(output_path, _asm);
  assemble(asm_path);
}
