#pragma once
#include "../cutils/s8s.c"

void exit_with_msgs(s8s msgs, int error_code)
{
  puts("Error(s):");
  foreach(msg, msgs, { printf("- %s \n", c(*msg)); });
  puts("\nExiting.");
  exit(error_code);
}
