#pragma once
#include "../cutils/dynamic.c"
#include "../cutils/s8.c"
#include "../cutils/types.c"
#include "instruction_stream.c"
#include "utils.c"
#include "computer.c"

typedef s8 str;
typedef str (*instruction_fn)(arena *, InstructionStream *, s8, Computer*);



typedef struct {
  u8 no_bits;
  b8 is_code;
  u8 bits_as_u8;
} Bits;
deflist(Bits, BitsList);


typedef struct {
  s8 name;
  instruction_fn creator;
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


void add_indices_for_decoder(arena s, Decoder d, InstructionTable *table) {
  TableIndices new_indices = {0};

  u8 total_width = 0;
  foreach (b, d.bit_description, total_width += b->no_bits);
  
  assert(total_width == NO_BITS_IN_BYTE);
  u8 current_end = 0;
  for (u32 bi = 0; bi < d.bit_description.len; ++bi) {
    Bits b = d.bit_description.data[bi];
    current_end += b.no_bits;
    u8 len_indices = new_indices.len;
    if (b.is_code) {
      u8 val = (b.bits_as_u8 << (NO_BITS_IN_BYTE - b.no_bits));
      if (len_indices)
        foreach (ti, new_indices, ti->index |= val);
      else
        *push(&s, &new_indices) = (TableIndex){val, d };
    } else {
      u8 no_variables = 1 << b.no_bits;
      // starting at one to avoid doulbe entries
      for (u8 i = len_indices ? 1 : 0; i < no_variables; ++i) {
        u8 val = (i << (NO_BITS_IN_BYTE - current_end));
        if (len_indices) {
          for (u32 ret_i = 0; ret_i < len_indices; ++ret_i) {
            *push(&s, &new_indices) =
                (TableIndex){new_indices.data[ret_i].index | val, d };
          }
        } else {
          *push(&s, &new_indices) = (TableIndex){val, d };
        }
      }
    }
  }

  foreach (index, new_indices, {
    if (!table->decoders[index->index].unknown)
      exit_with_msgs(make_s8_array(&s, d.name), 1);
    
    table->decoders[index->index] = index->decoder;

  });
}

u8 single_masks[] = {0, 1, 2, 4, 8, 16, 32, 64, 128};
u8 cover_masks[] = {0, 1, 3, 7, 15, 31, 63, 127, 255};

#define BIT(which_bit, value)                                                  \
  ((value) & single_masks[(which_bit)]) >> ((which_bit) - 1)
#define BRANGE(higher_bit, lower_bit, value)                                   \
  (((value) & cover_masks[(higher_bit)] & ~cover_masks[(lower_bit) - 1]) >>    \
   ((lower_bit) - 1))

#define ADD_OP(inst_fn, bit_descriptions)                                      \
  current_decoder = (Decoder){s(#inst_fn), &(inst_fn)};                        \
  bit_descriptions;                                                            \
  add_indices_for_decoder(s, current_decoder, &ret)

#define FIXED(no_bits, pattern)                                                \
  *push(&s, &current_decoder.bit_description) =                                \
      (Bits){(no_bits), true, (pattern)};

#define VAR(no_bits)                                                           \
  *push(&s, &current_decoder.bit_description) = (Bits){(no_bits)};





Register register_field_encoding(u8 reg, b8 w, Computer* computer) {
  w = !w; // this way the "table" below corresponds to table 4-9 in the 8086
          // instruction manual
#define R(reg)                                                            \
  (Register) { s(#reg), (u8*)(&computer->registers.reg), }
  
  switch (reg) {
  case 0b000:
    return w ? R(al) : R(ax);
  case 0b001:
    return w ? R(cl) : R(cx);
  case 0b010:
    return w ? R(dl) : R(dx);
  case 0b011:
    return w ? R(bl) : R(bx);
  case 0b100:
    return w ? R(ah) : R(sp);
  case 0b101:
    return w ? R(ch) : R(bp);
  case 0b110:
    return w ? R(dh) : R(si);
  case 0b111:
    return w ? R(bh) : R(di);
  default:
    exit_with_msg(s("REG is unparsable."), 1);
    return (Register) {0};
  }

#undef R  
}

#define INST_FN(name)                           \
  s8 name(arena *a, InstructionStream *is, s8 desc, Computer* computer)

INST_FN(mov_reg_mem_to_from_reg) {
  
  u8 byte1 = pop(is);
  u8 byte2 = pop(is);

  u8 W = BIT(1, byte1);
  u8 reg_is_destination = BIT(2, byte1);

  u8 mod = BRANGE(8, 7, byte2);
  u8 reg = BRANGE(6, 4, byte2);
  u8 r_m = BRANGE(3, 1, byte2);

  Register r_reg = register_field_encoding(reg, W, computer);
  Register r_r_m = register_field_encoding(r_m, W, computer);

  Register dest = reg_is_destination ? r_reg : r_r_m;
  Register src = reg_is_destination ? r_r_m : r_reg;
  
  return s8printf(a, "mov %s, %s", c(dest.name), c(src.name));
}

INST_FN(mov_im_to_reg_mem) {
  return s8("");
}

INST_FN(mov_im_to_reg) {
  return s8("");
}

INST_FN(mov_mem_to_acc) {
  return s8("");
}

INST_FN(mov_acc_to_mem) {
  return s8("");
}


InstructionTable create_8086_instruction_table(arena s) {
  InstructionTable ret = {0};
  for (u32 i = 0; i < 256; ++i)
    ret.decoders[i] = (Decoder){.unknown = true};

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
