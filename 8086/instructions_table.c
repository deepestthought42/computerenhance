#pragma once
#include "../cutils/dynamic.c"
#include "../cutils/error.c"
#include "../cutils/s8.c"
#include "../cutils/types.c"
#include "computer.c"
#include "instruction_stream.c"
#include "utils.c"

RESULT(s8, InstRes, INST_OK, INST_ERROR, INST_DATA_ERROR, INST_SRC_DATA_ERROR,
  INST_LOC_ERROR, INST_REG_FIELD_ERROR);

typedef s8 str;

typedef InstRes (*instruction_fn)(
  s8, arena*, InstructionStream*, s8, Computer*);

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
  s8 asm_inst;
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

#define ADD_OP(inst, inst_fn, bit_descriptions)                                \
  current_decoder                                                              \
    = (Decoder) { s(#inst_fn), &(inst_fn), .asm_inst = s(#inst) };             \
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

#define RE(reg, wide)                                                          \
  (MemoryLocation)                                                             \
  {                                                                            \
    .type = LOC_EFFECTIVE_ONE, .location = {                                   \
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

RESULT(i16, ReadDataRes, DATA_OK, DATA_ERROR);

ReadDataRes read_data_from_instruction(
  InstructionStream* is, b8 read_two_bytes, b8 sign_extension)
{
  /* printf("Reading %d byte(s) of data from stream %s sign extension.\n", */
  /*   read_two_bytes ? 2 : 1, sign_extension ? "w/" : "w/o"); */

  if (!read_two_bytes) {
    return OK_ReadDataRes((i16)((i8)(pop(is))));
  } else if (read_two_bytes && !sign_extension) {
    return OK_ReadDataRes(*((i16*)(pop_n(is, 2).data)));
  } else {
    return ERR_ReadDataRes(DATA_ERROR,
      s("reading two bytes with sign extension does not make sense."));
  }
}

void create_effective_address_table(arena* a, Computer* computer)
{
  MemoryLocation _locs[8][3] = {
    { R2(bx, si), R2D(bx, si, 0), R2D(bx, si, 1) },
    { R2(bx, di), R2D(bx, di, 0), R2D(bx, di, 1) },
    { R2(bp, si), R2D(bp, si, 0), R2D(bp, si, 1) },
    { R2(bp, di), R2D(bp, di, 0), R2D(bp, di, 1) },
    { RE(si, 1), R1D(si, 0), R1D(si, 1) },
    { RE(di, 1), R1D(di, 0), R1D(di, 1) },
    { (MemoryLocation) { .type = LOC_DIRECT_ADDRESS,
        .effective = { .displacement_is_wide = true } },
      R1D(bp, 0), R1D(bp, 1) },
    { RE(bx, 1), R1D(bx, 0), R1D(bx, 1) },
  };

  memcpy(
    computer->effective_address_table, _locs, 8 * 3 * sizeof(MemoryLocation));
}

RESULT(MemoryLocation, MemLocRes, MEMLOC_OK, MEMLOC_ERR,
  MEM_LOC_UNKNOWN_FIELD_ENCODING);

MemLocRes effective_address_calculation(
  u8 r_m, u8 mod, InstructionStream* is, Computer* computer)
{

  MemoryLocation loc = computer->effective_address_table[r_m][mod];
  switch (loc.type) {
  case LOC_REGISTER:
    return ERR_MemLocRes(MEMLOC_ERR,
      s("LOC_REGISTER not a valid type when calculating effective address."));
  case LOC_EFFECTIVE_TWO:
  case LOC_EFFECTIVE_ONE:
    // no need to read data
    break;
  case LOC_EFFECTIVE_ONE_W_DISP:
  case LOC_EFFECTIVE_TWO_W_DISP:
  case LOC_DIRECT_ADDRESS: {
    /* printf("Reading %d byte(s) of displacement from stream.\n", */
    /*   loc.effective.displacement_is_wide ? 2 : 1); */

    ReadDataRes rd = read_data_from_instruction(
      is, loc.effective.displacement_is_wide, false);
    if (rd.status == MEMLOC_OK)
      loc.effective.displacement_or_data = rd.v;
    else
      return ERR_MemLocRes(MEMLOC_ERR, rd.error_msg);
    break;
  }
  }
  return OK_MemLocRes(loc);
}

MemLocRes register_field_encoding(u8 reg, b8 w, Computer* computer)
{
  w = !w; // this way the "table" below corresponds to table 4-9 in the 8086
          // instruction manual

  switch (reg) {
  case 0b000:
    return OK_MemLocRes(w ? R(al, false) : R(ax, true));
  case 0b001:
    return OK_MemLocRes(w ? R(cl, false) : R(cx, true));
  case 0b010:
    return OK_MemLocRes(w ? R(dl, false) : R(dx, true));
  case 0b011:
    return OK_MemLocRes(w ? R(bl, false) : R(bx, true));
  case 0b100:
    return OK_MemLocRes(w ? R(ah, false) : R(sp, true));
  case 0b101:
    return OK_MemLocRes(w ? R(ch, false) : R(bp, true));
  case 0b110:
    return OK_MemLocRes(w ? R(dh, false) : R(si, true));
  case 0b111:
    return OK_MemLocRes(w ? R(bh, false) : R(di, true));
  default:
    return ERR_MemLocRes(MEM_LOC_UNKNOWN_FIELD_ENCODING,
      s8printf(GLOBAL_A, "Unknown field register field encoding: %s",
        print_bits(GLOBAL_A, reg, 3)));
  }
}
#undef R
#undef L

s8 print_memory_location(arena* a, MemoryLocation* loc)
{
  switch (loc->type) {
  case LOC_REGISTER:
    return loc->location.name;
  case LOC_EFFECTIVE_ONE:
    return s8printf(a, "[%s]", loc->location.name);
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

typedef struct {
  MemoryLocation src;
  MemoryLocation dst;
} SrcAndDst;

RESULT(SrcAndDst, SrcAndDstRes, SRC_DST_OK, SRC_DST_ERR);

SrcAndDstRes src_dst_for_reg_mem_to_from_either(
  Computer* computer, InstructionStream* is, u8 byte1, u8 byte2)
{

  u8 mod = BRANGE(8, 7, byte2);
  u8 reg = BRANGE(6, 4, byte2);
  u8 r_m = BRANGE(3, 1, byte2);

  b8 D = BIT(2, byte1);
  b8 W = BIT(1, byte1);

#define CHECK(var)                                                             \
  if ((var).status != MEMLOC_OK) {                                             \
    return ERR_SrcAndDstRes(SRC_DST_ERR, (var).error_msg);                     \
  }

  SrcAndDst ret = { 0 };
  if (mod == 0b11) {
    MemLocRes r_reg = register_field_encoding(reg, W, computer);
    CHECK(r_reg);
    MemLocRes r_r_m = register_field_encoding(r_m, W, computer);
    CHECK(r_r_m);

    ret.dst = D ? r_reg.v : r_r_m.v;
    ret.src = D ? r_r_m.v : r_reg.v;
  } else {
    MemLocRes r_reg = register_field_encoding(reg, W, computer);
    CHECK(r_reg);
    MemLocRes eff = effective_address_calculation(r_m, mod, is, computer);
    CHECK(eff);
    ret.dst = D ? r_reg.v : eff.v;
    ret.src = D ? eff.v : r_reg.v;
  }

  return OK_SrcAndDstRes(ret);
#undef CHECK
}

#define INST_FN(name)                                                          \
  InstRes name(                                                                \
    s8 asm_inst, arena* a, InstructionStream* is, s8 desc, Computer* computer)

#define INST_OK(s)                                                             \
  (InstRes) { INST_OK, (s) }

INST_FN(reg_mem_to_from_reg)
{
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);

  SrcAndDstRes sd
    = src_dst_for_reg_mem_to_from_either(computer, is, byte1, byte2);
  if (sd.status != SRC_DST_OK)
    return ERR_InstRes(INST_LOC_ERROR, sd.error_msg);

  return INST_OK(s8printf(a, "%s %s, %s", c(asm_inst),
    c(print_memory_location(a, &sd.v.dst)),
    c(print_memory_location(a, &sd.v.src))));
}

INST_FN(im_to_reg)
{
  u32 pos = is->current_pos;

  u8 byte1 = pop(is);
  b8 W = BIT(4, byte1);

  MemLocRes r_reg = register_field_encoding(BRANGE(3, 1, byte1), W, computer);
  if (r_reg.status != MEMLOC_OK)
    return ERR_InstRes(INST_REG_FIELD_ERROR, r_reg.error_msg);

  s8 ret = { 0 };

  ReadDataRes data = read_data_from_instruction(is, W, false);
  if (data.status != DATA_OK)
    return ERR_InstRes(INST_DATA_ERROR, r_reg.error_msg);

  return INST_OK(
    s8printf(a, "%s %s, %d", c(asm_inst), c(r_reg.v.location.name), data.v));
}

INST_FN(im_to_reg_mem)
{
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);

  b8 W = BIT(1, byte1);

  u8 mod = BRANGE(8, 7, byte2);
  u8 r_m = BRANGE(3, 1, byte2);

  MemLocRes dst = effective_address_calculation(r_m, mod, is, computer);
  if (dst.status != MEMLOC_OK)
    return ERR_InstRes(INST_LOC_ERROR, dst.error_msg);

  ReadDataRes data = read_data_from_instruction(is, W, false);
  if (data.status != DATA_OK)
    return ERR_InstRes(INST_DATA_ERROR, data.error_msg);

  return INST_OK(s8printf(a, "%s %s, %s %d", c(asm_inst),
    c(print_memory_location(a, &dst.v)), W ? "word" : "byte", data.v));
}

INST_FN(mem_to_acc)
{
  b8 W = BIT(1, pop(is));
  ReadDataRes data = read_data_from_instruction(is, W, false);
  if (data.status != DATA_OK)
    return ERR_InstRes(INST_DATA_ERROR, data.error_msg);

  return INST_OK(s8printf(a, "%s ax, [%d]", c(asm_inst), data.v));
}

INST_FN(acc_to_mem)
{
  b8 W = BIT(1, pop(is));
  ReadDataRes data = read_data_from_instruction(is, W, false);
  if (data.status != DATA_OK)
    return ERR_InstRes(INST_DATA_ERROR, data.error_msg);

  return INST_OK(s8printf(a, "%s [%d], ax", c(asm_inst), data.v));
}

INST_FN(reg_mem_w_reg_to_either)
{
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);

  SrcAndDstRes sd
    = src_dst_for_reg_mem_to_from_either(computer, is, byte1, byte2);
  if (sd.status != SRC_DST_OK)
    return ERR_InstRes(INST_SRC_DATA_ERROR, sd.error_msg);

  return INST_OK(s8printf(a, "%s %s, %s", c(asm_inst),
    c(print_memory_location(a, &sd.v.dst)),
    c(print_memory_location(a, &sd.v.src))));
}

typedef struct {
  MemoryLocation dst;
  b8 read_two_bytes;
  b8 sign_extended;
  s8 description;
} ImmediateToLoc;

RESULT(ImmediateToLoc, ImmediateToLocRes, IMTOLOC_OK,
  IMTOLOC_FIELD_ENCODING_ERR, IMTOLOC_EFFECTIVE_ADDR_ERR);

ImmediateToLocRes dst_for_im_to_reg_mem_with_s(arena* a,

  Computer* computer, InstructionStream* is, u8 byte1, u8 byte2)
{
  ImmediateToLoc ret = { 0 };
  b8 W = BIT(1, byte1);
  b8 S = BIT(2, byte1);
  u8 mod = BRANGE(8, 7, byte2);
  u8 reg = BRANGE(6, 4, byte2);
  u8 r_m = BRANGE(3, 1, byte2);

  ret.read_two_bytes = W && !S;
  ret.sign_extended = S;
  MemLocRes loc;
  if (mod == 0b11) {
    loc = register_field_encoding(r_m, W, computer);
    if (loc.status != MEMLOC_OK)
      return ERR_ImmediateToLocRes(IMTOLOC_FIELD_ENCODING_ERR, loc.error_msg);
    ret.dst = loc.v;
  } else {
    loc = effective_address_calculation(r_m, mod, is, computer);
    if (loc.status != MEMLOC_OK)
      return ERR_ImmediateToLocRes(IMTOLOC_EFFECTIVE_ADDR_ERR, loc.error_msg);
    ret.dst = loc.v;
  }

  ret.description = s8printf(a, "%s|%s .. W=%d, S=%d, mod=%s, reg=%s, r_m=%s",
    c(print_bits(a, byte1, 8)), c(print_bits(a, byte2, 8)), W, S,
    c(print_bits(a, mod, 2)), c(print_bits(a, reg, 3)),
    c(print_bits(a, r_m, 3)));

  return OK_ImmediateToLocRes(ret);
}

INST_FN(x_im_to_reg_mem)
{
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);
  ImmediateToLocRes loc
    = dst_for_im_to_reg_mem_with_s(a, computer, is, byte1, byte2);

  if (loc.status != IMTOLOC_OK)
    return ERR_InstRes(INST_LOC_ERROR, loc.error_msg);

  ReadDataRes read_data
    = read_data_from_instruction(is, loc.v.read_two_bytes, loc.v.sign_extended);

  if (read_data.status == DATA_OK)
    return OK_InstRes(s8printf(a, "%s %s %s, %d", c(asm_inst),
      loc.v.read_two_bytes || loc.v.sign_extended ? "word" : "byte",
      c(print_memory_location(a, &loc.v.dst)), read_data.v));
  else
    return ERR_InstRes(INST_DATA_ERROR, read_data.error_msg);
}

INST_FN(im_from_acc)
{
  b8 W = BIT(1, pop(is));
  ReadDataRes data = read_data_from_instruction(is, W, false);
  if (data.status != DATA_OK)
    return ERR_InstRes(INST_DATA_ERROR, data.error_msg);

  return OK_InstRes(s8printf(a, "%s ax, %d", c(asm_inst), data.v));
}

INST_FN(im_to_reg_mem_op_in_byte2)
{
  u8 byte2 = peek_by_n(is, 1);
  u8 reg = BRANGE(6, 4, byte2);
  switch (reg) {
  case 0b000:
    return x_im_to_reg_mem(s("add"), a, is, desc, computer);
  case 0b101:
    return x_im_to_reg_mem(s("sub"), a, is, desc, computer);
  case 0b111:
    return x_im_to_reg_mem(s("cmp"), a, is, desc, computer);

  default:
    return ERR_InstRes(INST_REG_FIELD_ERROR,
      s8printf(a, "Cannot decide based on reg: %s.", print_bits(a, reg, 3)));
  }
}

InstructionTable create_8086_instruction_table(arena s)
{
  InstructionTable ret = { 0 };
  for (u32 i = 0; i < 256; ++i)
    ret.decoders[i] = (Decoder) { .unknown = true };

  Decoder current_decoder = { 0 };

  // move

  ADD_OP(mov, reg_mem_to_from_reg, FIXED(6, 0b100010) VAR(2));
  ADD_OP(mov, im_to_reg_mem, FIXED(7, 0b1100011) VAR(1));
  ADD_OP(mov, im_to_reg, FIXED(4, 0b1011) VAR(4));
  ADD_OP(mov, mem_to_acc, FIXED(7, 0b1010000) VAR(1));
  ADD_OP(mov, acc_to_mem, FIXED(7, 0b1010001) VAR(1));

  ADD_OP(add, reg_mem_w_reg_to_either, FIXED(6, 0b000000) VAR(2));
  ADD_OP(add, im_from_acc, FIXED(7, 0b0000010) VAR(1));

  ADD_OP(sub, reg_mem_w_reg_to_either, FIXED(6, 0b001010) VAR(2));
  ADD_OP(sub, im_from_acc, FIXED(7, 0b0010110) VAR(1));

  ADD_OP(cmp, reg_mem_w_reg_to_either, FIXED(6, 0b001110) VAR(2));
  ADD_OP(cmp, im_from_acc, FIXED(7, 0b0011110) VAR(1));

  ADD_OP(_, im_to_reg_mem_op_in_byte2, FIXED(6, 0b100000) VAR(2));

  return ret;
}

#undef ADD_OP
#undef FIXED
#undef VAR
