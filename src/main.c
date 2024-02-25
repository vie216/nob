#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "defs.h"
#include "str.h"
#include "log.h"
#include "parser.h"
#include "checker.h"
#include "gen.h"

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
  fseek(file, 0, SEEK_END);
  content.len = ftell(file);
  content.ptr = malloc(content.len);
  fseek(file, 0, SEEK_SET);
  fread(content.ptr, 1, content.len, file);
  fclose(file);

  return content;
}

char *write_asm(char *path, char *_asm) {
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
  fprintf(file, _asm);
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

int main(int argc, char **argv) {
  char *input_path, *output_path;
  parse_args(argc, argv, &input_path, &output_path);
  Str source_code = read_file(input_path);

  INFO("Parsing\n");
  Expr program = parse_program(source_code, input_path);
  INFO("Type checking\n");
  Functions funcs = add_metadata(&program);
  INFO("Compiling\n");
  char *_asm = gen_linux_x86_64(funcs);
  INFO("Assembling\n");
  char *asm_path = write_asm(output_path, _asm);
  assemble(asm_path);
}
