/*
 * Copyright (c) 2012-2017, RISE SICS AB
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[])
{
  const char *filename;
  FILE *in_fp;
  FILE *out_fp;
  int ch;
  int width;

  if(argc != 2) {
    fprintf(stderr, "Usage: %s <file>\n", argv[0]);
    return EXIT_FAILURE;
  }

  filename = argv[1];

  in_fp = fopen(filename, "r");
  out_fp = stdout;

  if(in_fp == NULL || out_fp == NULL) {
    fprintf(stderr, "failed to open %s\n", in_fp ? "app.c" : filename);
    return EXIT_FAILURE;;
  }

  fprintf(out_fp,
          "/* Application bytecode compiled from the source file %s. */\n",
          filename);

  fprintf(out_fp, "static const uint8_t vm_program[] = {\n");

  width = 0;
  while((ch = fgetc(in_fp)) != EOF) {
    if(ferror(in_fp) || ferror(out_fp)) {
      goto io_error;
    }
    if(width == 10) {
      fprintf(out_fp, ",\n");
      width = 0;
    } else if(width > 0) {
      fprintf(out_fp, ", ");
    }
    width++;
    fprintf(out_fp, "0x%02x", ch);
  }

  fprintf(out_fp, "};\n");
  if(ferror(out_fp)) {
    goto io_error;
  }
  fclose(in_fp);
  fclose(out_fp);

  return EXIT_SUCCESS;
io_error:
  fprintf(stderr, "Failed to create an array of a VM program: I/O error\n");
  return EXIT_FAILURE;
}
