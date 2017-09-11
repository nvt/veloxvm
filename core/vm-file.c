/*
 * Copyright (c) 2012-2017, RISE SICS AB.
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
 * 3. Neither the name of the Institute nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS
 * IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * INSTITUTE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * \file
 *         An implementation of a file system interface that reads
 *         only from a predetermined VM application image.
 * \author
 *         Niclas Finne <nfi@sics.se>
 *         Joakim Eriksson <joakime@sics.se>
 *         Nicolas Tsiftes <nvt@acm.org>
 */

#include "vm.h"
#ifdef VM_BUNDLE
#include "vm-app-image.h"
#endif
#include "vm-file.h"

#include <string.h>

#ifdef VM_BUNDLE
#define VM_FILE_SIZE ((vm_file_offset_t)sizeof(vm_program))
#else
#define VM_FILE_SIZE 0
#endif

static vm_file_offset_t pos;

int
vm_file_open(const char *n, int flags)
{
#ifndef VM_BUNDLE
  return -1;
#endif

  VM_DEBUG(VM_DEBUG_MEDIUM, "Opening a program stored in memory (%u bytes)",
           (unsigned)VM_FILE_SIZE);

  pos = 0;
  return 1;
}

void
vm_file_close(int fd)
{
}

int
vm_file_read(int fd, void *buf, vm_file_offset_t len)
{
  (void)fd;

#ifdef VM_BUNDLE
  if(pos < VM_FILE_SIZE) {
    if(len > VM_FILE_SIZE - pos) {
      len = VM_FILE_SIZE - pos;
    }
    memcpy(buf, &vm_program[pos], len);
    pos += len;
    return len;
  }
#endif

  return -1;
}

int
vm_file_write(int fd, const void *buf, vm_file_offset_t len)
{
  (void)fd;
  (void)buf;
  (void)len;

  return -1;
}

vm_file_offset_t
vm_file_seek(int fd, vm_file_offset_t offset, int whence)
{
#ifndef VM_BUNDLE
  return (vm_file_offset_t)-1;
#endif

  switch(whence) {
  case VM_FILE_SEEK_SET:
    pos = offset;
    break;
  case VM_FILE_SEEK_END:
    pos = VM_FILE_SIZE - offset;
    break;
  case VM_FILE_SEEK_CUR:
    pos += offset;
    break;
  default:
    return (vm_file_offset_t)-1;
  }

  if(pos < 0 || pos > VM_FILE_SIZE) {
    pos = 0;
    return (vm_file_offset_t)-1;
  }

  return pos;
}

int
vm_file_remove(const char *name)
{
  return -1;
}
