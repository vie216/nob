#include <stdlib.h>

#include "arena.h"

#define ARENA_CAP 256

typedef struct Arena Arena;

struct Arena {
  u8     buffer[ARENA_CAP];
  u32    used;
  Arena *next;
};

static Arena *arena = NULL;

static Arena *create_arena() {
  Arena *_arena = malloc(sizeof(Arena));
  _arena->used = 0;
  _arena->next = NULL;
  return _arena;
}

void *aalloc(u32 amount) {
  if (amount > ARENA_CAP)
    return NULL;
  if (!arena)
    arena = create_arena();

  Arena *_arena = arena;

  while (_arena->used + amount > ARENA_CAP) {
    if (!_arena->next)
      _arena->next = create_arena();
    _arena = _arena->next;
  }

  u32 used = _arena->used;
  _arena->used += amount;
  return _arena->buffer + used;
}
