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
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#ifndef VM_PORT_H
#define VM_PORT_H

#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>

#define VM_POSIX_LISTEN_SLOTS 10

#define VM_MALLOC malloc
#define VM_FREE free
#define VM_REALLOC realloc
#define VM_STRDUP strdup

typedef int vm_loader_handle_t;
typedef off_t vm_loader_offset_t;

#if VM_BUNDLE
#include "vm-file.h"
#define VM_LOADER_OPEN(name) vm_file_open((name), O_RDONLY)
#define VM_LOADER_READ(handle, buf, size) vm_file_read((handle), (buf), (size))
#define VM_LOADER_SEEK_RELATIVE(handle, offset) \
  vm_file_seek((handle), (offset), VM_FILE_SEEK_CUR)
#define VM_LOADER_SEEK_ABSOLUTE(handle, offset) \
  vm_file_seek((handle), (offset), VM_FILE_SEEK_SET)
#define VM_LOADER_CLOSE(handle) vm_file_close(handle)
#else
#define VM_LOADER_OPEN(name) open((name), O_RDONLY)
#define VM_LOADER_READ(handle, buf, size) read((handle), (buf), (size))
#define VM_LOADER_SEEK_RELATIVE(handle, offset) \
  lseek((handle), (offset), SEEK_CUR)
#define VM_LOADER_SEEK_ABSOLUTE(handle, offset) \
  lseek((handle), (offset), SEEK_SET)
#define VM_LOADER_CLOSE(handle) close(handle)
#endif

#define VM_MEMPOOL_CONF_ALLOC malloc
#define VM_MEMPOOL_CONF_FREE free

#define VM_RANDOM_FUNCTION random

typedef uint64_t vm_native_time_t;
/* Unimplemented. Use clock_gettime() later. */
#define VM_NATIVE_TIME() vm_port_time()
/* The native clocks resolution in Hertz. */
#define VM_NATIVE_TIME_RESOLUTION() vm_port_time_resolution()

typedef struct vm_native_port {
  int fd;
  unsigned flags;
  char buf[BUFSIZ];
} vm_native_port_t;

vm_native_time_t vm_port_time(void);
vm_native_time_t vm_port_time_resolution(void);

struct vm_thread;

void vm_posix_syscall_error(struct vm_thread *, const char *);

int vm_posix_poll_port(struct vm_thread *, int, int);
void vm_posix_unpoll_port(int);

#endif /* !VM_PORT_H */
