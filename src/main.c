#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "defs.h"
#include "str.h"
#include "log.h"
#include "parser.h"
#include "type.h"
#include "gen-linux-x86_64.h"

void parse_args(int argc, char **argv,
                char **input_path,
                char **output_path) {
  *input_path = NULL;
  *output_path = "out";
  i32 i = 1;
  while (i < argc) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'o') {
        if (i + 1 >= argc) {
          ERROR("-o flag expects an argument\n");
          exit(1);
        }

        *output_path = argv[++i];
      } else {
        ERROR("Unknown flag: %s\n", argv[i]);
        exit(1);
      }
    } else {
      if (*input_path) {
        ERROR("Multiple input files are not supported\n");
        exit(1);
      }

      *input_path = argv[i];
    }

    i++;
  }

  if (!*input_path) {
    ERROR("Input file is not provided\n");
    exit(1);
  }
}

Str read_file(char *path) {
  Str content;

  FILE *file = fopen(path, "r");
  if (!file) {
    ERROR("Could not open input file\n");
    exit(1);
  }

  fseek(file, 0, SEEK_END);
  content.len = ftell(file);
  content.ptr = malloc(content.len);
  fseek(file, 0, SEEK_SET);
  fread(content.ptr, 1, content.len, file);
  fclose(file);

  return content;
}

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

Def *intrinsic_defs(void) {
  Def *defs = NULL;
  Def *arg_defs = NULL;
  Def *arg_defs_end = NULL;

  LL_PREPEND(arg_defs, arg_defs_end, Def);
  LL_PREPEND(arg_defs, arg_defs_end, Def);

  TypeInt *eent = aalloc(sizeof(TypeInt));
  eent->kind = IntKindS64;
  arg_defs->type = (Type) { TypeKindInt, { .eent = eent } };

  eent = aalloc(sizeof(TypeInt));
  eent->kind = IntKindS64;
  arg_defs_end->type = (Type) { TypeKindInt, { .eent = eent } };

  TypeFunc *func = aalloc(sizeof(TypeFunc));
  eent = aalloc(sizeof(TypeInt));
  eent->kind = IntKindS64;
  func->result_type = (Type) { TypeKindInt, { .eent = eent } };
  func->arg_defs = arg_defs;
  func->arity = 2;
  Type bin_op = { TypeKindFunc, { .func = func } };

  LL_APPEND(defs, Def);
  defs->name = STR_LIT("+");
  defs->type = bin_op;
  defs->is_intrinsic = true;

  LL_APPEND(defs, Def);
  defs->name = STR_LIT("-");
  defs->type = bin_op;
  defs->is_intrinsic = true;

  LL_APPEND(defs, Def);
  defs->name = STR_LIT("*");
  defs->type = bin_op;
  defs->is_intrinsic = true;

  LL_APPEND(defs, Def);
  defs->name = STR_LIT("/");
  defs->type = bin_op;
  defs->is_intrinsic = true;

  LL_APPEND(defs, Def);
  defs->name = STR_LIT("%");
  defs->type = bin_op;
  defs->is_intrinsic = true;

  arg_defs = aalloc(sizeof(Def));
  TypePtr *ptr = aalloc(sizeof(TypePtr));
  eent = aalloc(sizeof(TypeInt));
  eent->kind = IntKindU8;
  ptr->points_to = (Type) { TypeKindInt, { .eent = eent } };
  ptr->is_str_lit = true;
  arg_defs->type = (Type) { TypeKindPtr, { .ptr = ptr } };

  func = aalloc(sizeof(TypeFunc));
  func->result_type = (Type) { TypeKindUnit };
  func->arg_defs = arg_defs;
  func->arity = 1;
  Type _asm = { TypeKindFunc, { .func = func } };

  LL_APPEND(defs, Def);
  defs->name = STR_LIT("asm");
  defs->type = _asm;
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
