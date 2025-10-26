#pragma once
#include "error.c"


RESULT(s8, file, file_OK, file_NOT_FOUND, file_WRONG_SIZE, file_SEEK_ERROR)

file s8slurp_file(arena *a, s8 filename)
{
  s8 ret;
  FILE *fp = fopen(c(filename), "r");

  if (fp == NULL)
    return (file){file_NOT_FOUND, s8printf(a, "File not found")};

  if (fseek(fp, 0L, SEEK_END) == 0) {
    long bufsize = ftell(fp);
    if (bufsize == -1)
      return (file){file_WRONG_SIZE,
                    s8printf(a, "Couldn't determine file size")};

    /* Go back to the start of the file. */
    if (fseek(fp, 0L, SEEK_SET) != 0)
      return (file){file_SEEK_ERROR,
                    s8printf(a, "Couldn't seek to beginning of file")};

    ret = empty_s8(a, bufsize);
    size_t len = fread(ret.data, sizeof(u8), bufsize, fp);
  }
  fclose(fp);
  return (file){file_OK, ret};
}
