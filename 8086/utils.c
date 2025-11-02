#pragma once
#include "../cutils/s8.c"
#include "../cutils/s8s.c"


#define BOLD  "\e[1m"
#define BOLD_RED "\033[1;31m"
#define OFF   "\e[m"



void exit_with_msgs(s8s msgs, int error_code)
{
  puts(BOLD_RED "Error(s):" OFF);
  foreach(msg, msgs, printf("%s \n", c(*msg)););
  puts("\nExiting.");
  exit(error_code);
}

void exit_with_msg(s8 msg, int error_code)
{
  exit_with_msgs(make_s8_array((arena*)&global_arena__, msg), error_code);
}


