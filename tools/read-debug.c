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

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_FORM_SIZE 8192

static int
read_form(int fd, char *buf, int max_size)
{
  int length;

  if(read(fd, buf, 1) != 1) {
    return 0;
  }

  length = (int)((unsigned)buf[0] & 0xff);
  if(length > max_size - 1) {
    return 0;
  }

  if(read(fd, buf, length) != length) {
    return 0;
  }
  buf[length] = '\0';

  return 1;
}

int
main(int argc, char *argv[])
{
  int fd;
  char buf[MAX_FORM_SIZE];
  int expr_count;
  int i;
  int form_id;
  const char *filename;

  if(argc < 2 || argc > 3) {
    fprintf(stderr, "Usage: %s <file> [<form-id>]\n", argv[0]);
    return EXIT_FAILURE;
  }

  filename = argv[1];
  form_id = argc == 3 ? atoi(argv[2]) : -1;

  fd = open(filename, O_RDONLY);

  if(read(fd, buf, 1) != 1) {
    fprintf(stderr, "Failed to read the expression count!\n");
    return EXIT_FAILURE;
  }
  expr_count = (int)((unsigned)buf[0] & 0xff);

  if(form_id + 1 > expr_count) {
    fprintf(stderr, "Form ID %d is over the maximum %d!\n",
            form_id, expr_count - 1);
    return EXIT_FAILURE;
  }

  for(i = 0; i < expr_count; i++) {
    if(read_form(fd, buf, sizeof(buf))) {
      if(form_id == -1 || form_id == i) {
        printf("Form %d: %s\n", i, buf);
        if(form_id == i) {
          break;
        }
      }
    } else {
      fprintf(stderr, "Failed to read form %d!\n", i);
      break;
    }
  }

  close(fd);

  return EXIT_SUCCESS;
}
