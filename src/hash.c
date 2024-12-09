#include "hash.h"

i32 hash_str(Str str) {
  i32 hash = 14523;

  for (i32 i = 0; i < str.len; ++i) {
    hash *= 64;
    hash += str.ptr[i];
  }

  return hash;
}

i32 hash_type(Type type) {
  i32 hash;

  switch (type.kind) {
  case TypeKindUnit: {
    hash = 690;
  } break;

  case TypeKindInt: {
    hash = 5342;
  } break;

  case TypeKindPtr: {
    hash = hash_type(type.as.ptr->points_to) * 453;
  } break;

  case TypeKindFunc: {
    hash = 56973 + hash_str(type.as.func->name);

    Def *arg_def = type.as.func->arg_defs;
    while (arg_def) {
      hash *= 12;
      hash += hash_str(arg_def->name);

      arg_def = arg_def->next;
    }
  } break;
  }

  return hash;
}
