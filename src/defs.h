#ifndef DEFS_H
#define DEFS_H

#include <stdlib.h>

#define ARRAY_LEN(array) (sizeof(array) / sizeof(array[0]))

#define DA_APPEND(da, element)                                        \
  do {                                                                \
    if ((da).cap <= (da).len) {                                       \
      if ((da).cap != 0) {                                            \
        while ((da).cap <= (da).len)                                  \
          (da).cap *= 2;                                              \
        (da).items = realloc((da).items, sizeof(element) * (da).cap); \
      } else {                                                        \
        (da).cap = 1;                                                 \
        (da).items = malloc(sizeof(element));                         \
      }                                                               \
    }                                                                 \
    (da).items[(da).len++] = element;                                 \
  } while (0)

#define LL_APPEND(ll, type)           \
  do {                                \
    type *new = aalloc(sizeof(type)); \
    new->next = ll;                   \
    ll = new;                         \
  } while(0)

#define LL_PREPEND(ll, ll_end, type)  \
  do {                                \
    type *new = aalloc(sizeof(type)); \
    if (ll_end)                       \
      ll_end->next = new;             \
    else                              \
      ll = new;                       \
    ll_end = new;                     \
  } while(0)

typedef char           i8;
typedef unsigned char  u8;
typedef short          i16;
typedef unsigned short u16;
typedef int            i32;
typedef unsigned int   u32;
typedef long           i64;
typedef unsigned long  u64;

typedef float f32;

typedef u8 bool;

#define false 0
#define true  1

#include "arena.h"

#endif // DEFS_H
