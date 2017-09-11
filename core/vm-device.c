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
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include "vm.h"
#include "vm-log.h"

#define VM_DEVICE_PATH "/dev/"

static vm_device_t *devices;

int
vm_device_register(const char *name, const vm_port_io_t *io, uint8_t flags)
{
  vm_device_t *device;

  device = VM_MALLOC(sizeof(vm_device_t));
  if(device == NULL) {
    return 0;
  }
  device->name = VM_STRDUP(name);
  device->io = io;
  device->flags = flags;

  device->next = devices;
  devices = device;

  return 1;
}

vm_device_t *
vm_device_lookup(const char *path)
{
  vm_device_t *device;

  if(strncmp(path, VM_DEVICE_PATH, 5) == 0) {
    path += sizeof(VM_DEVICE_PATH) - 1;
    for(device = devices; device != NULL; device = device->next) {
      /* TODO: Split the path and do a tree search. */
      if(strcmp(device->name, path) == 0) {
        return device;
      }
    }
  }

  return NULL;
}

vm_device_t *
vm_device_get_all(void)
{
  return devices;
}
