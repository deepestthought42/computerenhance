#pragma once
#include "../cutils/s8.c"
#include "../cutils/types.c"

#define CAT(a, b) a##b
#define DBLREG(name)                                                           \
    union {                                                                    \
        u16 CAT(name, x);                                                      \
        struct {                                                               \
            u8 CAT(name, h);                                                   \
            u8 CAT(name, l);                                                   \
        };                                                                     \
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
} Register;

typedef struct {
    Registers registers;
} Computer;
