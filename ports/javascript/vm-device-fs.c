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
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "vm.h"
#include "vm-log.h"

static int
fs_open(vm_port_t *port, const char *filename, uint8_t flags)
{
  int fs_flags;

  switch(flags) {
  case VM_PORT_FLAG_INPUT:
    fs_flags = O_RDONLY;
    break;
  case VM_PORT_FLAG_OUTPUT:
    fs_flags = O_WRONLY;
    break;
  default:
    fs_flags = O_RDWR;
    break;
  }

  return open(filename, fs_flags | O_CREAT | O_NONBLOCK, S_IRUSR | S_IWUSR);
}

static int
fs_read(vm_port_t *port, char *buf, size_t size)
{
  return read(port->fd, buf, size);
}

static int
fs_write(vm_port_t *port, const char *buf, size_t size)
{
  return write(port->fd, buf, size);
}

static void
fs_close(vm_port_t *port)
{
  close(port->fd);
}

const vm_port_io_t device_fs = {
  .open = fs_open,
  .read = fs_read,
  .write = fs_write,
  .close = fs_close
};
