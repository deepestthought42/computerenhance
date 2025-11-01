#pragma once
#include "../cutils/types.c"

struct {
  union {
    u16 AX;
    struct {
      u8 AL;
      u8 AH;
    };
  };
} registers_8086 = {0};
