#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"
#include "log.h"
#include "arena.h"

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
    ERROR("Input file was not provided\n");
    exit(1);
  }
}

Str read_file(char *path) {
  Str content;

  FILE *file = fopen(path, "r");
  if (!file) {
    ERROR("Could not open input file: %s\n", path);
    exit(1);
  }

  fseek(file, 0, SEEK_END);
  content.len = ftell(file);
  content.ptr = aalloc(content.len);
  fseek(file, 0, SEEK_SET);
  fread(content.ptr, 1, content.len, file);
  fclose(file);

  return content;
}
