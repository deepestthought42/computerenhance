#pragma once
#include "utils.c"

deflist(u8, bytes);

typedef struct {
  u8 *data;
  u32 len;
} byte_slice;

typedef struct {
  bytes instructions;
  u32 current_pos;
} InstructionStream;

u8 current(InstructionStream *s) {
  return s->instructions.data[s->current_pos];
}

u8 next(InstructionStream *stream) {
  if (stream->current_pos + 1 > stream->instructions.len) {
    exit_with_msg(GLOBAL_A, "Cannot read %d bytes from instruction stream", n), 1);
  }
  return stream->instructions.data[stream->current_pos++];
}

byte_slice next_slice(InstructionStream *stream, u32 n) {
  if (stream->current_pos + n > stream->instructions.len) {
    exit_with_msg(
        s8printf(GLOBAL_A, "Cannot read %d bytes from instruction stream", n),
        1);
  }

  u32 current_pos = stream->current_pos;
  stream->current_pos += n;
  return (byte_slice){&stream->instructions.data[current_pos], n};
}
