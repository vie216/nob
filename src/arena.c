#include <stdlib.h>

#include "arena.h"

#define DEFAULT_ARENA_CAP 256

typedef struct Arena Arena;

struct Arena {
  u8    *buffer;
  u32    cap, len;
  Arena *next;
};

static Arena *arena = NULL;
static Arena *last_arena = NULL;

static Arena *create_arena(u32 cap) {
  Arena *_arena = malloc(sizeof(Arena) + cap);
  _arena->buffer = (u8 *) _arena + sizeof(Arena);
  _arena->cap = cap;
  _arena->len = 0;
  _arena->next = NULL;
  last_arena = _arena;
  return _arena;
}

void *aalloc(u32 amount) {
  if (amount > DEFAULT_ARENA_CAP)
    arena = create_arena(amount);
  if (!arena)
    arena = create_arena(DEFAULT_ARENA_CAP);

  Arena *_arena = arena;

  while (_arena->len + amount > _arena->cap) {
    if (!_arena->next)
      _arena->next = create_arena(DEFAULT_ARENA_CAP);
    _arena = _arena->next;
  }

  u32 len = _arena->len;
  _arena->len += amount;
  return _arena->buffer + len;
}
