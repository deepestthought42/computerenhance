#include <stdio.h>
#include <stdlib.h>

#define OPTPARSE_IMPLEMENTATION
#define OPTPARSE_API static
#include "../cutils/files.c"
#include "../cutils/optparse.h"
#include "../cutils/s8s.c"
#include "computer.c"
#include "instructions_table.c"
#include "utils.c"

typedef struct {
  s8 input_path;
  s8 output_path;
} Arguments;

Arguments get_cmd_line_arguments(arena* a, int argc, char** argv)
{
  struct optparse_long longopts[] = { { "input", 'i', OPTPARSE_REQUIRED },
    { "output", 'o', OPTPARSE_REQUIRED },
    {
      "trap_on_error",
      't',
      OPTPARSE_OPTIONAL
    },
    { 0 } };

  Arguments retval = { 0 };
  char* arg;
  int option;
  struct optparse options;

  (void)argc;
  optparse_init(&options, argv);
  while ((option = optparse_long(&options, longopts, NULL)) != -1) {
    switch (option) {
    case 'i':
      retval.input_path = s8copy(a, s(options.optarg));
      break;
    case 'o':
      retval.output_path = s8copy(a, s(options.optarg));
      break;
    case 't':
      __trap_on_error = true;
      break;
    case '?':
      fprintf(stderr, "%s: %s\n", argv[0], options.errmsg);
      exit(EXIT_FAILURE);
    }
  }
  s8s error_msgs = { 0 };
  if (is_empty_s8(retval.input_path))
    *push(a, &error_msgs) = s("No input path given");

  if (is_empty_s8(retval.output_path))
    *push(a, &error_msgs) = s("No output path given");

  if (len(error_msgs) > 0)
    exit_with_msgs(error_msgs, 1);

  /* Print remaining arguments. */
  while ((arg = optparse_arg(&options)))
    printf("%s\n", arg);

  return retval;
}

void decode_to_file(arena s, Arguments args)
{
  file file = s8slurp_file(&s, args.input_path);
  if (file.status != file_OK)
    exit_with_msgs(make_s8_array(&s, file.error_msg), 1);

  InstructionTable table = create_8086_instruction_table(s);
  InstructionStream stream
    = { .instructions = { (u8*)file.v.data, file.v.len } };

  Computer c = { 0 };
  create_effective_address_table(&s, &c);

  s8s content = make_s8_array_cstrs(&s, "bits 16", "");

  while (more_in(stream)) {
    u8 index = peek(&stream);
    Decoder* d = &table.decoders[index];
    if (d->unknown) {
      s8s msgs = { 0 };
      *push(&s, &msgs)
        = s8printf(&s, "unknown instruction at byte position: %d :: %s",
                   stream.current_pos, c(print_bits(&s, index, 8)));
      *push(&s, &msgs) = s8printf(&s, "-- current content:");
      foreach(c, content, *push(&s, &msgs) = *c);
      exit_with_msgs(msgs, 1);
    }
    u32 pos_before = stream.current_pos;
    InstRes res = d->inst(d->asm_inst, &s, &stream, d->name, &c);
    if (res.status != INST_OK) {
      s8s msgs = { 0 };
      *push(&s, &msgs)
        = s8printf(&s, "Error from instruction: %s at byte position: %d",
          c(d->name), stream.current_pos);
      *push(&s, &msgs) = res.error_msg;

      *push(&s, &msgs) = s8printf(&s, "-- current content:");
      foreach(c, content, *push(&s, &msgs) = *c);
      exit_with_msgs(msgs, 1);
    }


    *push(&s, &content) = res.v.line;

    if (stream.current_pos == pos_before) {
      s8s msgs = { 0 };
      *push(&s, &msgs) = s8printf(&s,
        "instruction with name: %s at byte position: %d "
        "didn't consume any bytes.",
        c(d->name), stream.current_pos);
      *push(&s, &msgs) = s8printf(&s, "-- current content:");
      foreach(c, content, *push(&s, &msgs) = *c);
      exit_with_msgs(msgs, 1);
    }
  }

  write_strings_to_file(args.output_path, content);
}

int main(int argc, char** argv)
{
  initialize_global_s8_arena(1 * MB);
  arena a = newarena(1 * GB);
  Arguments args = get_cmd_line_arguments(&a, argc, argv);

  printf("Trying to decode:\n" BOLD "%s" OFF "\n", c(args.input_path));
  decode_to_file(a, args);
  printf("Successfully decoded to:\n%s\n\n", c(args.output_path));

  return 0;
}
