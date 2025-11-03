#pragma once
#include "../cutils/dynamic.c"
#include "../cutils/s8.c"
#include "../cutils/types.c"
#include "computer.c"
#include "instruction_stream.c"
#include "utils.c"

typedef s8 str;
typedef str (*instruction_fn)(arena*, InstructionStream*, s8, Computer*);

typedef struct {
  u8 no_bits;
  b8 is_code;
  u8 bits_as_u8;
} Bits;
deflist(Bits, BitsList);

typedef struct {
  s8 name;
  instruction_fn inst;
  BitsList bit_description;
  b8 unknown;
} Decoder;
deflist(Decoder, Decoders);

typedef struct {
  u8 index;
  Decoder decoder;
} TableIndex;
deflist(TableIndex, TableIndices);

typedef struct {
  Decoder decoders[256];
} InstructionTable;

#define NO_BITS_IN_BYTE 8

void add_indices_for_decoder(arena s, Decoder d, InstructionTable* table)
{
  TableIndices new_indices = { 0 };

  u8 total_width = 0;
  foreach(b, d.bit_description, total_width += b->no_bits);

  assert(total_width == NO_BITS_IN_BYTE);
  u8 current_end = 0;
  for (u32 bi = 0; bi < d.bit_description.len; ++bi) {
    Bits b = d.bit_description.data[bi];
    current_end += b.no_bits;
    u8 len_indices = new_indices.len;
    if (b.is_code) {
      u8 val = (b.bits_as_u8 << (NO_BITS_IN_BYTE - b.no_bits));
      if (len_indices)
        foreach(ti, new_indices, ti->index |= val);
      else
        *push(&s, &new_indices) = (TableIndex) { val, d };
    } else {
      u8 no_variables = 1 << b.no_bits;
      // starting at one to avoid doulbe entries
      for (u8 i = len_indices ? 1 : 0; i < no_variables; ++i) {
        u8 val = (i << (NO_BITS_IN_BYTE - current_end));
        if (len_indices) {
          for (u32 ret_i = 0; ret_i < len_indices; ++ret_i) {
            *push(&s, &new_indices)
              = (TableIndex) { new_indices.data[ret_i].index | val, d };
          }
        } else {
          *push(&s, &new_indices) = (TableIndex) { val, d };
        }
      }
    }
  }

  foreach(index, new_indices, {
    if (!table->decoders[index->index].unknown)
      exit_with_msg(
        s8printf(&s, "Index: %d (0b%s) has already been assigned to: %s",
          index->index, c(print_bits(&s, index->index, 8)), c(d.name)),
        1);

    table->decoders[index->index] = index->decoder;
  });
}

u8 single_masks[] = { 0, 1, 2, 4, 8, 16, 32, 64, 128 };
u8 cover_masks[] = { 0, 1, 3, 7, 15, 31, 63, 127, 255 };

#define BIT(which_bit, value)                                                  \
  ((value) & single_masks[(which_bit)]) >> ((which_bit) - 1)
#define BRANGE(higher_bit, lower_bit, value)                                   \
  (((value) & cover_masks[(higher_bit)] & ~cover_masks[(lower_bit) - 1])       \
    >> ((lower_bit) - 1))

#define ADD_OP(inst_fn, bit_descriptions)                                      \
  current_decoder = (Decoder) { s(#inst_fn), &(inst_fn) };                     \
  bit_descriptions;                                                            \
  add_indices_for_decoder(s, current_decoder, &ret)

#define FIXED(no_bits, pattern)                                                \
  *push(&s, &current_decoder.bit_description)                                  \
    = (Bits) { (no_bits), true, (pattern) };

#define VAR(no_bits)                                                           \
  *push(&s, &current_decoder.bit_description) = (Bits) { (no_bits) };

#define R(reg, wide)                                                           \
  (MemoryLocation)                                                             \
  {                                                                            \
    .type = LOC_REGISTER, .location = {                                        \
      s(#reg),                                                                 \
      (u8*)(&computer->registers.reg),                                         \
      (wide),                                                                  \
    }                                                                          \
  }

#define R2(reg1, reg2)                                                         \
  (MemoryLocation)                                                             \
  {                                                                            \
    .type = LOC_EFFECTIVE_TWO,                                                 \
    .effective                                                                 \
      = {.location1 = { s(#reg1), (u8*)(&computer->registers.reg1), true },    \
          .location2 = { s(#reg2), (u8*)(&computer->registers.reg2), true } }  \
  }

#define R1D(reg1, d_is_wide)                                                   \
  (MemoryLocation)                                                             \
  {                                                                            \
    .type = LOC_EFFECTIVE_ONE_W_DISP, .effective = {                           \
      .displacement_is_wide = (d_is_wide),                                     \
      .location1 = { s(#reg1), (u8*)(&computer->registers.reg1), true },       \
    }                                                                          \
  }

#define R2D(reg1, reg2, d_is_wide)                                             \
  (MemoryLocation)                                                             \
  {                                                                            \
    .type = LOC_EFFECTIVE_TWO_W_DISP,                                          \
    .effective                                                                 \
      = {.displacement_is_wide = (d_is_wide),                                  \
          .location1 = { s(#reg1), (u8*)(&computer->registers.reg1) },         \
          .location2 = { s(#reg2), (u8*)(&computer->registers.reg2) } }        \
  }

i16 read_instruction_as_data(InstructionStream* is, b8 is_wide)
{
  if (is_wide)
    return *((i16*)(pop_n(is, 2).data));
  else
    return (i16)((i8)(pop(is)));
}

void create_effective_address_table(arena* a, Computer* computer)
{
  MemoryLocation _locs[8][3] = {
    { R2(bx, si), R2D(bx, si, 0), R2D(bx, si, 1) },
    { R2(bx, di), R2D(bx, di, 0), R2D(bx, di, 0) },
    { R2(bp, si), R2D(bp, si, 0), R2D(bp, si, 0) },
    { R2(bp, di), R2D(bp, di, 0), R2D(bp, di, 0) },
    { R(si, 1), R1D(si, 0), R1D(si, 1) },
    { R(di, 1), R1D(di, 0), R1D(di, 1) },
    { (MemoryLocation) { .type = LOC_DIRECT_ADDRESS,
        .effective = { .displacement_is_wide = true } },
      R1D(bp, 0), R1D(bp, 1) },
    { R(bx, 1), R1D(bx, 0), R1D(bx, 1) },
  };

  memcpy(
    computer->effective_address_table, _locs, 8 * 3 * sizeof(MemoryLocation));
}

MemoryLocation effective_address_calculation(
  u8 r_m, u8 mod, InstructionStream* is, Computer* computer)
{

  MemoryLocation loc = computer->effective_address_table[r_m][mod];
  switch (loc.type) {
  case LOC_EFFECTIVE_ONE_W_DISP:
  case LOC_EFFECTIVE_TWO_W_DISP:
  case LOC_DIRECT_ADDRESS:
    loc.effective.displacement_or_data
      = read_instruction_as_data(is, loc.effective.displacement_is_wide);
    break;
  default:
    break;
  }
  return loc;
}

MemoryLocation register_field_encoding(u8 reg, b8 w, Computer* computer)
{
  w = !w; // this way the "table" below corresponds to table 4-9 in the 8086
          // instruction manual

  switch (reg) {
  case 0b000:
    return w ? R(al, false) : R(ax, true);
  case 0b001:
    return w ? R(cl, false) : R(cx, true);
  case 0b010:
    return w ? R(dl, false) : R(dx, true);
  case 0b011:
    return w ? R(bl, false) : R(bx, true);
  case 0b100:
    return w ? R(ah, false) : R(sp, true);
  case 0b101:
    return w ? R(ch, false) : R(bp, true);
  case 0b110:
    return w ? R(dh, false) : R(si, true);
  case 0b111:
    return w ? R(bh, false) : R(di, true);
  default:
    exit_with_msg(s("REG is unparsable."), 1);
    return (MemoryLocation) { 0 };
  }
}
#undef R
#undef L

s8 print_memory_location(arena* a, MemoryLocation* loc)
{
  switch (loc->type) {
  case LOC_REGISTER:
    return loc->location.name;
  case LOC_DIRECT_ADDRESS:
    return s8printf(a, "[%d]", loc->effective.displacement_or_data);
  case LOC_EFFECTIVE_TWO:
    return s8printf(a, "[%s + %s]", c(loc->effective.location1.name),
      c(loc->effective.location2.name));
  case LOC_EFFECTIVE_ONE_W_DISP:
    if (loc->effective.displacement_or_data != 0)
      return s8printf(a, "[%s %s %d]", c(loc->effective.location1.name),
        loc->effective.displacement_or_data < 0 ? "-" : "+",
        abs(loc->effective.displacement_or_data));
    else
      return s8printf(a, "[%s]", c(loc->effective.location1.name));

  case LOC_EFFECTIVE_TWO_W_DISP:
    if (loc->effective.displacement_or_data != 0)
      return s8printf(a, "[%s + %s %s %d]", c(loc->effective.location1.name),
        c(loc->effective.location2.name),
        loc->effective.displacement_or_data < 0 ? "-" : "+",
        abs(loc->effective.displacement_or_data));
    else
      return s8printf(a, "[%s + %s]", c(loc->effective.location1.name),
        c(loc->effective.location2.name));
  }
}

#define INST_FN(name)                                                          \
  s8 name(arena* a, InstructionStream* is, s8 desc, Computer* computer)

INST_FN(mov_reg_mem_to_from_reg)
{
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);

  u8 W = BIT(1, byte1);
  u8 reg_is_destination = BIT(2, byte1);

  u8 mod = BRANGE(8, 7, byte2);
  u8 reg = BRANGE(6, 4, byte2);
  u8 r_m = BRANGE(3, 1, byte2);
  MemoryLocation src, dst;

  if (mod == 0b11) {
    MemoryLocation r_reg = register_field_encoding(reg, W, computer);
    MemoryLocation r_r_m = register_field_encoding(r_m, W, computer);

    dst = reg_is_destination ? r_reg : r_r_m;
    src = reg_is_destination ? r_r_m : r_reg;
  } else {
    MemoryLocation r_reg = register_field_encoding(reg, W, computer);
    MemoryLocation eff = effective_address_calculation(r_m, mod, is, computer);
    dst = reg_is_destination ? r_reg : eff;
    src = reg_is_destination ? eff : r_reg;
  }

  return s8printf(a, "mov %s, %s", c(print_memory_location(a, &dst)),
    c(print_memory_location(a, &src)));
}

INST_FN(mov_im_to_reg)
{
  u32 pos = is->current_pos;

  u8 byte1 = pop(is);
  b8 W = BIT(4, byte1);

  MemoryLocation r_reg
    = register_field_encoding(BRANGE(3, 1, byte1), W, computer);
  s8 ret = { 0 };
  i16 data = read_instruction_as_data(is, W);

  return s8printf(a, "mov %s, %d", c(r_reg.location.name), data);
}

INST_FN(mov_im_to_reg_mem)
{
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);

  b8 W = BIT(1, byte1);

  u8 mod = BRANGE(8, 7, byte2);
  u8 r_m = BRANGE(3, 1, byte2);

  MemoryLocation dst = effective_address_calculation(r_m, mod, is, computer);
  i16 data = read_instruction_as_data(is, W);
  return s8printf(a, "mov %s, %s %d", c(print_memory_location(a, &dst)),
    W ? "word" : "byte", data);
}

INST_FN(mov_mem_to_acc)
{
  b8 W = BIT(1, pop(is));
  return s8printf(a, "mov ax, [%d]", read_instruction_as_data(is, W));
}

INST_FN(mov_acc_to_mem)
{
  b8 W = BIT(1, pop(is));
  return s8printf(a, "mov [%d], ax", read_instruction_as_data(is, W));
}

INST_FN(add_reg_mem_w_reg_to_either) { return s8(""); }
INST_FN(add_im_to_reg_mem) { return s8(""); }

INST_FN(add_im_to_acc) {
    b8 W = BIT(1, pop(is));
    return s8printf(a, "add ax, [%d]", read_instruction_as_data(is, W));
}

InstructionTable create_8086_instruction_table(arena s)
{
  InstructionTable ret = { 0 };
  for (u32 i = 0; i < 256; ++i)
    ret.decoders[i] = (Decoder) { .unknown = true };

  Decoder current_decoder = { 0 };

  // move

  ADD_OP(mov_reg_mem_to_from_reg, FIXED(6, 0b100010) VAR(2));
  ADD_OP(mov_im_to_reg_mem, FIXED(7, 0b1100011) VAR(1));
  ADD_OP(mov_im_to_reg, FIXED(4, 0b1011) VAR(4));
  ADD_OP(mov_mem_to_acc, FIXED(7, 0b1010000) VAR(1));
  ADD_OP(mov_acc_to_mem, FIXED(7, 0b1010001) VAR(1));
  
  ADD_OP(add_reg_mem_w_reg_to_either, FIXED(6, 0b000000) VAR(2));
  ADD_OP(add_im_to_reg_mem, FIXED(6, 0b100000) VAR(2));
  ADD_OP(add_im_to_acc, FIXED(6, 0b0000010) VAR(2));

  return ret;
}
#undef ADD_OP
#undef FIXED
#undef VAR
