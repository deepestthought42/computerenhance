#include <stdio.h>
#include <stdlib.h>

#define OPTPARSE_IMPLEMENTATION
#define OPTPARSE_API static
#include "../cutils/optparse.h"
#include "../cutils/s8s.c"
#include "../cutils/files.c"
#include "utils.c"
#include "instructions_table.c"



typedef struct {
  s8 input_path;
  s8 output_path;
} Arguments;


Arguments get_cmd_line_arguments(arena* a, int argc, char **argv)
{
    struct optparse_long longopts[] = {
      {"input", 'i', OPTPARSE_REQUIRED}, 
      {"output", 'o', OPTPARSE_REQUIRED},
      {0}
  };

  Arguments retval = {0};
  char *arg;
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
      
    case '?':
      fprintf(stderr, "%s: %s\n", argv[0], options.errmsg);
      exit(EXIT_FAILURE);
    }
  }
  s8s error_msgs = {0};
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





void decode(arena* a, arena s, Arguments args)
{
  file file = s8slurp_file(a, args.input_path);
  if (file.status != file_OK)
    exit_with_msgs(make_s8_array(a, file.error_msg), 1);

  InstructionTable table = create_8086_instruction_table(s);
  InstructionStream stream = { .instructions = { (u8*)file.v.data, file.v.len } };
  
  
  
}



int main(int argc, char **argv) {
  initialize_global_s8_arena(1*MB);
  arena a = newarena(1*GB);
  arena scratch = newarena(1*GB);
  Arguments args = get_cmd_line_arguments(&a, argc, argv);
  decode(&a, scratch, args);
  return 0;
}
