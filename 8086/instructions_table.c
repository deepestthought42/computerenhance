#pragma once
#include "../cutils/dynamic.c"
#include "../cutils/s8.c"
#include "../cutils/types.c"
#include "utils.c"
#include "instruction_stream.c"


typedef s8 str;
typedef str (*create_asm_line)(arena *, InstructionStream *, s8);

typedef struct {
  create_asm_line encodings[256];
  s8 descriptions[256];
} InstructionTable;

typedef struct {
  u8 index;
  create_asm_line creator;
} TableIndex;

typedef struct {
  u8 no_bits;
  b8 is_code;
  u8 bits_as_u8;
} Bits;

deflist(Bits, BitsList);
deflist(TableIndex, TableIndices);

typedef struct {
  s8 name;
  create_asm_line creator;
  BitsList bit_description;
} Decoder;

deflist(Decoder, Decoders);

#define NO_BITS_IN_BYTE 8

s8 unknown_instruction(arena *a, InstructionStream *is, s8 description) {
  exit_with_msgs(make_s8_array(a, description), 1);
  return (s8){0};
}

void add_indices_for_decoder(arena s, Decoder d, InstructionTable *table) {
  TableIndices new_indices = {0};

  u8 total_width = 0;
  foreach (b, d.bit_description, total_width += b->no_bits)
    ;
  assert(total_width == NO_BITS_IN_BYTE);
  u8 current_end = 0;
  for (u32 bi = 0; bi < d.bit_description.len; ++bi) {
    Bits b = d.bit_description.data[bi];
    current_end += b.no_bits;
    u8 len_indices = new_indices.len;
    if (b.is_code) {
      u8 val = (b.bits_as_u8 << (NO_BITS_IN_BYTE - b.no_bits));
      if (len_indices)
        foreach (ti, new_indices, ti->index |= val)
          ;
      else
        *push(&s, &new_indices) = (TableIndex){val, d.creator};
    } else {
      u8 no_variables = 1 << b.no_bits;
      // starting at one to avoid doulbe entries
      for (u8 i = len_indices ? 1 : 0; i < no_variables; ++i) {
        u8 val = (i << (NO_BITS_IN_BYTE - current_end));
        if (len_indices) {
          for (u32 ret_i = 0; ret_i < len_indices; ++ret_i) {
            *push(&s, &new_indices) =
                (TableIndex){new_indices.data[ret_i].index | val, d.creator};
          }
        } else {
          *push(&s, &new_indices) = (TableIndex){val, d.creator};
        }
      }
    }
  }

  foreach (index, new_indices, {
    if (table->encodings[index->index] != &unknown_instruction)
      exit_with_msgs(make_s8_array(&s, d.name), 1);
    table->encodings[index->index] = index->creator;
    table->descriptions[index->index] = d.name;
  })
    ;
}

u8 single_masks[] = { 0, 1, 2, 4, 8, 16, 32, 64, 128 };
u8 cover_masks[] = { 0, 1, 3, 7, 15, 31, 63, 127, 255 };


#define BIT(which_bit, value)                                                  \
  ((value) & single_masks[(which_bit)]) >> ((which_bit) - 1)
#define BRANGE(lower_bit, higher_bit, value)                                   \
  (((value) & cover_masks[(higher_bit)] & ~cover_masks[(lower_bit) - 1]) >>    \
   ((lower_bit) - 1))

s8 register_field_encoding(u8 reg, b8 w) {
  w = !w; // this way the "table" below corresponds to table 4-9 in the 8086
          // instruction manual
  switch (reg) {
  case 0b000:
    return w ? s("AL") : s("AX");
  case 0b001:
    return w ? s("CL") : s("CX");
  case 0b010:
    return w ? s("DL") : s("DX");
  case 0b011:
    return w ? s("BL") : s("BX");
  case 0b100:
    return w ? s("AH") : s("SP");
  case 0b101:
    return w ? s("CH") : s("BP");
  case 0b110:
    return w ? s("DH") : s("SI");
  case 0b111:
    return w ? s("BH") : s("DI");
  default:
    exit_with_msg(s("REG is unparsable.") , 1);
    return s(""); // muffle compiler warning
  }
}

s8 mov_reg_mem_to_from_reg(arena *a, InstructionStream *is, s8 desc) {
  printf("Found mov_reg_mem_to_from_reg.\n");
  u8 b1 = next(is);
  u8 b2 = next(is);
  
  u8 W = BIT(1, b1);
  u8 D = BIT(2, b2);

  u8 mod = BRANGE(7, 8, b2);
  u8 reg = BRANGE(4, 6, b2);
  u8 r_m = BRANGE(1, 3, b2);
  
  return s8("");
}

s8 mov_im_to_reg_mem(arena *a, InstructionStream *is, s8 desc) {
  return s8("");
}

s8 mov_im_to_reg(arena *a, InstructionStream *is, s8 desc) { return s8(""); }

s8 mov_mem_to_acc(arena *a, InstructionStream *is, s8 desc) { return s8(""); }

s8 mov_acc_to_mem(arena *a, InstructionStream *is, s8 desc) { return s8(""); }

#define ADD_OP(inst_fn, bit_descriptions)                                      \
  current_decoder = (Decoder) { s(#inst_fn), &(inst_fn) };                     \
  bit_descriptions;                                                            \
  add_indices_for_decoder(s, current_decoder, &ret)

#define FIXED(no_bits, pattern)                                                \
  *push(&s, &current_decoder.bit_description) =                                \
      (Bits){(no_bits), true, (pattern)};
#define VAR(no_bits)                                                           \
  *push(&s, &current_decoder.bit_description) = (Bits){(no_bits)};

InstructionTable create_8086_instruction_table(arena s) {
  InstructionTable ret = {0};
  for (u32 i = 0; i < 256; ++i) {
    ret.encodings[i] = &unknown_instruction;
    ret.descriptions[i] = s("unknown instruction");
  }
  Decoder current_decoder = {0};


  // move

  ADD_OP(mov_reg_mem_to_from_reg, FIXED(6, 0b100010) VAR(2));
  /* ADD_OP(mov_im_to_reg_mem, FIXED(7, 0b1100011) VAR(1)); */
  /* ADD_OP(mov_im_to_reg, FIXED(4, 0b1011) VAR(4)); */

  return ret;
}
#undef ADD_OP
#undef FIXED
#undef VAR
