/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
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

/*
 * R7RS bytevector ports (open-input-bytevector, open-output-bytevector,
 * get-output-bytevector). Same shape as core/vm-device-string.c but
 * the snapshot returns a vm_vector_t with VM_VECTOR_FLAG_BUFFER. No
 * VM_STRING_MAX_LENGTH cap applies; vector length is int32_t.
 */

#include <stdlib.h>
#include <string.h>

#include "vm.h"
#include "vm-log.h"
#include "vm-native.h"

typedef struct {
  uint8_t *buf;
  size_t capacity;
  size_t length;
  size_t pos;
} bytevector_port_state_t;

#define INITIAL_OUTPUT_CAPACITY 64

static int
bv_read(vm_port_t *port, char *buf, size_t size)
{
  bytevector_port_state_t *s = port->opaque_desc;
  size_t available;

  if(s == NULL || s->pos >= s->length) {
    return 0;
  }
  available = s->length - s->pos;
  if(size > available) {
    size = available;
  }
  memcpy(buf, s->buf + s->pos, size);
  s->pos += size;
  return (int)size;
}

static int
bv_write(vm_port_t *port, const char *buf, size_t size)
{
  bytevector_port_state_t *s = port->opaque_desc;
  size_t needed;
  uint8_t *new_buf;
  size_t new_capacity;

  if(s == NULL) {
    return -1;
  }

  needed = s->length + size;
  if(needed > s->capacity) {
    new_capacity = s->capacity == 0 ? INITIAL_OUTPUT_CAPACITY : s->capacity;
    while(new_capacity < needed) {
      new_capacity *= 2;
    }
    new_buf = VM_REALLOC(s->buf, new_capacity);
    if(new_buf == NULL) {
      return -1;
    }
    s->buf = new_buf;
    s->capacity = new_capacity;
  }

  memcpy(s->buf + s->length, buf, size);
  s->length += size;
  return (int)size;
}

static void
bv_close(vm_port_t *port)
{
  bytevector_port_state_t *s = port->opaque_desc;
  if(s != NULL) {
    if(s->buf != NULL) {
      VM_FREE(s->buf);
    }
    VM_FREE(s);
    port->opaque_desc = NULL;
  }
}

const vm_port_io_t device_bytevector = {
  .open = NULL,
  .read = bv_read,
  .read_object = NULL,
  .write = bv_write,
  .close = bv_close
};

vm_port_t *
vm_bytevector_port_open_input(vm_thread_t *thread,
                              const uint8_t *src, size_t len)
{
  vm_port_t *port;
  bytevector_port_state_t *s;

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return NULL;
  }
  vm_port_register(port);

  s = VM_MALLOC(sizeof(bytevector_port_state_t));
  if(s == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return NULL;
  }
  s->buf = NULL;
  s->capacity = 0;
  s->length = len;
  s->pos = 0;
  if(len > 0) {
    s->buf = VM_MALLOC(len);
    if(s->buf == NULL) {
      VM_FREE(s);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return NULL;
    }
    memcpy(s->buf, src, len);
    s->capacity = len;
  }

  port->thread = thread;
  port->io = &device_bytevector;
  port->fd = -1;
  port->opaque_desc = s;
  port->flags |= VM_PORT_FLAG_OPEN | VM_PORT_FLAG_INPUT;
  return port;
}

vm_port_t *
vm_bytevector_port_open_output(vm_thread_t *thread)
{
  vm_port_t *port;
  bytevector_port_state_t *s;

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return NULL;
  }
  vm_port_register(port);

  s = VM_MALLOC(sizeof(bytevector_port_state_t));
  if(s == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return NULL;
  }
  s->buf = NULL;
  s->capacity = 0;
  s->length = 0;
  s->pos = 0;

  port->thread = thread;
  port->io = &device_bytevector;
  port->fd = -1;
  port->opaque_desc = s;
  port->flags |= VM_PORT_FLAG_OPEN | VM_PORT_FLAG_OUTPUT;
  return port;
}

int
vm_bytevector_port_get_output(vm_thread_t *thread, vm_port_t *port,
                              vm_obj_t *result)
{
  bytevector_port_state_t *s;
  vm_vector_t *vec;

  if(port->io != &device_bytevector ||
     VM_IS_CLEAR(port->flags, VM_PORT_FLAG_OUTPUT)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return 0;
  }

  s = port->opaque_desc;
  vec = vm_vector_create(result,
                         s == NULL ? 0 : (vm_integer_t)s->length,
                         VM_VECTOR_FLAG_BUFFER);
  if(vec == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return 0;
  }
  if(s != NULL && s->length > 0) {
    memcpy(vec->bytes, s->buf, s->length);
  }
  return 1;
}
