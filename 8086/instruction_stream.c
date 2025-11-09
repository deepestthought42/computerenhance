#pragma once
#include "../cutils/hash_table.c"
#include "../cutils/types.c"

#include "utils.c"

deflist(u8, bytes);

typedef struct {
  u8* data;
  u32 len;
} byte_slice;

u64 label_hash(u32 k) { return (u64)k; }
u64 label_equal(u32 a, u32 b) { return a == b; }
defhtable(Labels, u32, b8, label_hash, label_equal);

typedef struct {
  bytes instructions;
  u32 current_pos;
  // this doesn't really belong here but for now I am too lazy too create a
  // proper structure for it:
  Labels labels;
} InstructionStream;

b8 more_in(InstructionStream s) { return s.current_pos < s.instructions.len; }

u8 peek_by_n(InstructionStream* s, u32 nth)
{
  if (s->current_pos + nth > s->instructions.len) {
    exit_with_msg(s("Cannot read byte from instruction stream"), 1);
  }
  return s->instructions.data[s->current_pos + nth];
}

u8 peek(InstructionStream* s) { return peek_by_n(s, 0); }

u8 pop(InstructionStream* stream)
{
  if (stream->current_pos + 1 > stream->instructions.len) {
    exit_with_msg(s("Cannot read byte from instruction stream"), 1);
  }
  stream->current_pos += 1;
  return stream->instructions.data[stream->current_pos - 1];
}

byte_slice pop_n(InstructionStream* stream, u32 n)
{
  if (stream->current_pos + n > stream->instructions.len) {
    exit_with_msg(
      s8printf(GLOBAL_A, "Cannot read %d bytes from instruction stream", n), 1);
  }

  u32 current_pos = stream->current_pos;
  stream->current_pos += n;
  return (byte_slice) { &stream->instructions.data[current_pos], n };
}
