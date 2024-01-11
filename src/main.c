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
                char **input_file_path,
                char **output_file_path) {
  i32 i;

  *input_file_path = NULL;
  *output_file_path = "out.asm";
  i = 1;
  while (i < argc) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'o') {
        if (i + 1 >= argc) {
          ERROR("-o flag expects an argument\n");
          exit(1);
        }

        *output_file_path = argv[++i];
      } else {
        ERROR("Unknown flag: %s\n", argv[i]);
      }
    } else {
      if (*input_file_path) {
        ERROR("Multiple input files are not supported\n");
        exit(1);
      }

      *input_file_path = argv[i];
    }

    i++;
  }

  if (!*input_file_path) {
    ERROR("Input file is not provided\n");
    exit(1);
  }
}

int main(int argc, char **argv) {
  Expr program;
  char *input_file_path, *output_file_path;
  FILE *output_file;

  parse_args(argc, argv, &input_file_path, &output_file_path);

  program = parse_program(STR("", 0));
  output_file = fopen(output_file_path, "w");
  fprintf(output_file, gen_linux_x86_64(program));
  fclose(output_file);
}
