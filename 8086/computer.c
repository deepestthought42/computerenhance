#pragma once
#include "../cutils/types.c"
#include "../cutils/s8.c"


#define CAT(a, b) a##b
#define DBLREG(name) union { u16 CAT(name, X); struct { u8 CAT(name, H); u8 CAT(name, L); };};

typedef struct {
  DBLREG(A);
  DBLREG(B);
  DBLREG(C);
  DBLREG(D);

  u16 SP;
  u16 BP;
  u16 SI;
  u16 DI;
  
} Registers;

typedef struct {
  s8 name;
  u8 *location;
} Register;

typedef struct {
  Registers registers;
} Computer;
