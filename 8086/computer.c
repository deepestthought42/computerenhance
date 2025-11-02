#pragma once
#include "../cutils/s8.c"
#include "../cutils/types.c"

#define CAT(a, b) a##b
#define DBLREG(name)                                                           \
  union {                                                                      \
    u16 CAT(name, x);                                                          \
    struct {                                                                   \
      u8 CAT(name, h);                                                         \
      u8 CAT(name, l);                                                         \
    };                                                                         \
  };

typedef struct {
  DBLREG(a);
  DBLREG(b);
  DBLREG(c);
  DBLREG(d);

  u16 sp;
  u16 bp;
  u16 si;
  u16 di;

} Registers;

typedef struct {
  s8 name;
  u8* location;
  b8 is_wide;
} Register;

typedef enum LocationType {
  LOC_REGISTER = 0,
  LOC_EFFECTIVE_TWO = 1,
  LOC_EFFECTIVE_ONE_W_DISP = 2,
  LOC_EFFECTIVE_TWO_W_DISP = 3,
  LOC_DIRECT_ADDRESS = 4
} LocationType;

typedef struct {
  LocationType type;
  union {
    Register location;
    struct {
      Register location1;
      Register location2;
      i16 displacement_or_data;
      b8 displacement_is_wide;
    } effective;
  };
} MemoryLocation;

typedef struct {
  Registers registers;
  MemoryLocation effective_address_table[8][3];
} Computer;
