#pragma once
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "types.c"

#define kB (1 << 10)
#define MB (1 << 20)
#define GB (1 << 30)

// clang-format off
#define new(arena, type, count)                                               \
  (type*)alloc((arena), sizeof(type), _Alignof(type), count)
// clang-format on

typedef struct {
  byte *beg;
  byte *end;
} arena;

typedef struct {
  double v;
  char *unit;
} BytesWUnit;

#define ONE_OVER_LN_1024 1 / 6.9314718056

BytesWUnit get_w_unit(double v)
{
  i32 multiplier = (i32)(log(v) * ONE_OVER_LN_1024);
  BytesWUnit res = {v / pow(1024, multiplier)};
  switch (multiplier) {
  case 0: res.unit = "B"; break;
  case 1: res.unit = "kB"; break;
  case 2: res.unit = "MB"; break;
  case 3: res.unit = "GB"; break;
  case 4: res.unit = "TB"; break;
  default: res.unit = "B"; res.v = v;
  }
  return res;
}

void print_arena_info(arena *a)
{
  BytesWUnit av = get_w_unit(a->end - a->beg);
  printf("available in arena: %.3f %s\n", av.v, av.unit);
}

byte *alloc(arena *a, ptrdiff_t size, ptrdiff_t align, ptrdiff_t count)
{
  ptrdiff_t padding = -(ptrdiff_t)a->beg & (align - 1);
  ptrdiff_t available = a->end - a->beg - padding;
  if (available < 0 || count > available / size) {
    printf("arena out of memory, available: %ld, requested count: %ld of size: %ld",
           available, count, size);
    fflush(stdout);
    abort(); // one possible out-of-memory policy
  }
  void *p = a->beg + padding;
  a->beg += padding + count * size;
  return (byte *)memset(p, 0, count * size);
}

arena newarena(ptrdiff_t cap)
{
  arena a = {0};
  a.beg = malloc(cap);
  a.end = a.beg ? a.beg + cap : 0;
  return a;
}

arena newarena_a(arena *a, ptrdiff_t cap)
{
  arena ret = {0};
  ret.beg = (byte *)alloc(a, sizeof(byte), 1, cap);
  ret.end = ret.beg ? ret.beg + cap : 0;
  return ret;
}
