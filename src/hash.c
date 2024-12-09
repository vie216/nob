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

i32 hash_main_func(void) {
  Type main_func_type = {
    TypeKindFunc,
    { .func = aalloc(sizeof(TypeFunc)) },
    STR_LIT("8"),
  };
  main_func_type.as.func->name = STR_LIT("main");
  main_func_type.as.func->result_type = (Type) { TypeKindUnit };
  main_func_type.as.func->arg_defs = NULL;
  main_func_type.as.func->arity = 0;

  return hash_type(main_func_type);
}
