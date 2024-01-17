#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "defs.h"
#include "str.h"
#include "log.h"
#include "parser.h"
#include "gen.h"

void parse_args(int argc, char **argv,
                char **input_path,
                char **output_path) {
  i32 i;

  *input_path = NULL;
  *output_path = "out";
  i = 1;
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
  FILE *file;
  Str content;

  file = fopen(path, "r");
  fseek(file, 0, SEEK_END);
  content.len = ftell(file);
  content.ptr = malloc(content.len);
  fseek(file, 0, SEEK_SET);
  fread(content.ptr, 1, content.len, file);
  fclose(file);

  return content;
}

void write_asm(char *path, char *_asm) {
  i32 new_path_len;
  FILE *file;

  new_path_len = strlen(path) + 4;
  path = realloc(path, new_path_len);
  path[new_path_len - 4] = '.';
  path[new_path_len - 3] = 'a';
  path[new_path_len - 2] = 's';
  path[new_path_len - 1] = 'm';

  file = fopen(path, "w");
  fprintf(file, _asm);
  fclose(file);
}

void assemble(char *asm_path) {
  i32 len;
  char *cmd;

  len = strlen(asm_path);
  cmd = malloc(len + 5);
  for (i32 i = 0; i < 5; ++i)
    cmd[i] = "fasm "[i];
  for (i32 i = 0; i < len; ++i)
    cmd[i + 5] = asm_path[i];
  cmd[len + 5] = '\0';
  system(cmd);
}

int main(int argc, char **argv) {
  char *input_path, *output_path, *_asm;
  Str source_code;
  Expr program;

  parse_args(argc, argv, &input_path, &output_path);
  source_code = read_file(input_path);

  INFO("Parsing\n");
  program = parse_program(source_code, input_path);
  INFO("Compiling\n");
  _asm = gen_linux_x86_64(program);
  INFO("Assembling\n");
  write_asm(output_path, _asm);
  assemble(output_path);
}
