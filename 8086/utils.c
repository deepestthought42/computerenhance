#pragma once
#include "../cutils/s8.c"
#include "../cutils/s8s.c"

#define BOLD  "\e[1m"
#define BOLD_RED "\033[1;31m"
#define OFF   "\e[m"



void exit_with_msgs(s8s msgs, int error_code)
{
  puts("\n");
  puts(BOLD_RED "Error(s):" OFF);
  foreach(msg, msgs, printf("%s \n", c(*msg)););
  puts("\nExiting.");
  exit(error_code);
}

void exit_with_msg(s8 msg, int error_code)
{
  exit_with_msgs(make_s8_array((arena*)&global_arena__, msg), error_code);
}


void set_bit(u8* byte, u8 bit_position, u8 value) {
  if (value) {
    // Set the bit to 1
    *byte |= (1 << bit_position);
  } else {
    // Set the bit to 0
    *byte &= ~(1 << bit_position);
  }
}
