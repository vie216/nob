#ifndef DEFS_H
#define DEFS_H

#define ARRAY_LEN(array) (sizeof(array) / sizeof(array[0]))
#define DA_APPEND(da, element)                                        \
  do {                                                                \
    if ((da).cap <= (da).len) {                                       \
      if ((da).cap) {                                                 \
        while ((da).cap <= (da).len)                                  \
          (da).cap *= 2;                                              \
        (da).items = realloc((da).items, sizeof(element) * (da).cap); \
      } else {                                                        \
        (da).cap = 1;                                                 \
        (da).items = malloc(sizeof(element) * (da).cap);              \
      }                                                               \
    }                                                                 \
    (da).items[(da).len++] = element;                                 \
  } while (0)

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

#endif // DEFS_H
