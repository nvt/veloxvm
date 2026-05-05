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
 *
 * Author: Nicolas Tsiftes <nicolas.tsiftes@ri.se>
 */

/*
 * Encode a vm_obj_t into the runtime value wire format described in
 * doc/repl-design.md (used inside RESULT frames). The driver-side
 * decoder lives in tools/repl/velox_repl/render/decode.py and must
 * stay in sync with the tag list below.
 */

#ifdef VM_REPL_ENABLE

#include "vm.h"
#include "vm-log.h"
#include "vm-repl.h"
#include "vm-table.h"

#include <stdint.h>
#include <string.h>

#define TAG_NONE    0x01
#define TAG_BOOL    0x02
#define TAG_INT     0x03
#define TAG_REAL    0x04
#define TAG_CHAR    0x05
#define TAG_STRING  0x06
#define TAG_SYMBOL  0x07
#define TAG_LIST    0x08
#define TAG_VECTOR  0x09
#define TAG_PAIR    0x0A
#define TAG_OPAQUE  0xF0

typedef struct {
  uint8_t *buf;
  size_t cap;
  size_t pos;
  int overflow;
} encbuf_t;

static void
emit_byte(encbuf_t *e, uint8_t b)
{
  if(e->pos + 1 > e->cap) {
    e->overflow = 1;
    return;
  }
  e->buf[e->pos++] = b;
}

static void
emit_bytes(encbuf_t *e, const void *src, size_t n)
{
  if(e->pos + n > e->cap) {
    e->overflow = 1;
    return;
  }
  memcpy(e->buf + e->pos, src, n);
  e->pos += n;
}

static void
emit_u16_be(encbuf_t *e, uint16_t v)
{
  uint8_t b[2];
  b[0] = (uint8_t)((v >> 8) & 0xFF);
  b[1] = (uint8_t)(v & 0xFF);
  emit_bytes(e, b, 2);
}

static void
emit_u32_be(encbuf_t *e, uint32_t v)
{
  uint8_t b[4];
  b[0] = (uint8_t)((v >> 24) & 0xFF);
  b[1] = (uint8_t)((v >> 16) & 0xFF);
  b[2] = (uint8_t)((v >>  8) & 0xFF);
  b[3] = (uint8_t)( v        & 0xFF);
  emit_bytes(e, b, 4);
}

static void
emit_i64_be(encbuf_t *e, int64_t v)
{
  uint64_t u = (uint64_t)v;
  uint8_t b[8];
  int i;
  for(i = 7; i >= 0; i--) {
    b[i] = (uint8_t)(u & 0xFF);
    u >>= 8;
  }
  emit_bytes(e, b, 8);
}

static void
emit_string_payload(encbuf_t *e, const char *s, size_t n)
{
  if(n > UINT16_MAX) {
    n = UINT16_MAX;
  }
  emit_u16_be(e, (uint16_t)n);
  emit_bytes(e, s, n);
}

static void encode_value(encbuf_t *e, vm_program_t *program,
                         const vm_obj_t *obj, unsigned depth);

/* Emit a list as either LIST(count) for proper lists or a chain of
   PAIR(car, cdr) for improper ones. The VM_LIST_FLAG_PAIR flag marks
   a 2-element improper list; longer improper chains aren't currently
   constructed by VeloxVM, but the encoder would handle them by
   walking until the dotted tail. */
static void
encode_list(encbuf_t *e, vm_program_t *program,
            const vm_list_t *list, unsigned depth)
{
  vm_list_item_t *item;
  unsigned count;

  if(VM_IS_SET(list->flags, VM_LIST_FLAG_PAIR) &&
     list->head != NULL && list->head->next != NULL &&
     list->head->next->next == NULL) {
    /* Improper 2-element pair: emit as PAIR(car, cdr). */
    emit_byte(e, TAG_PAIR);
    encode_value(e, program, &list->head->obj, depth + 1);
    encode_value(e, program, &list->head->next->obj, depth + 1);
    return;
  }

  count = 0;
  for(item = list->head; item != NULL; item = item->next) {
    count++;
  }
  if(count > UINT16_MAX) {
    count = UINT16_MAX;
  }

  emit_byte(e, TAG_LIST);
  emit_u16_be(e, (uint16_t)count);
  for(item = list->head; item != NULL && count > 0; item = item->next) {
    encode_value(e, program, &item->obj, depth + 1);
    count--;
  }
}

static void
encode_vector(encbuf_t *e, vm_program_t *program,
              const vm_vector_t *vec, unsigned depth)
{
  vm_integer_t i;
  vm_integer_t len = vec->length;

  if(VM_IS_SET(vec->flags, VM_VECTOR_FLAG_BUFFER)) {
    /* Byte buffers travel as a STRING of raw bytes; the driver
       renders them with the bytes-style formatter. */
    size_t n = (len < 0 || (uint64_t)len > UINT16_MAX) ? UINT16_MAX
                                                       : (size_t)len;
    emit_byte(e, TAG_STRING);
    emit_u16_be(e, (uint16_t)n);
    emit_bytes(e, vec->bytes, n);
    return;
  }

  if(len < 0) {
    len = 0;
  }
  if((uint64_t)len > UINT16_MAX) {
    len = UINT16_MAX;
  }

  emit_byte(e, TAG_VECTOR);
  emit_u16_be(e, (uint16_t)len);
  for(i = 0; i < len; i++) {
    encode_value(e, program, &vec->elements[i], depth + 1);
  }
}

#define MAX_DEPTH 32

static void
encode_value(encbuf_t *e, vm_program_t *program,
             const vm_obj_t *obj, unsigned depth)
{
  if(depth > MAX_DEPTH) {
    /* Stop runaway recursion on cyclic / deeply nested structures. */
    emit_byte(e, TAG_OPAQUE);
    emit_string_payload(e, "<deep>", 6);
    return;
  }
  if(obj == NULL) {
    emit_byte(e, TAG_NONE);
    return;
  }

  switch(obj->type) {
  case VM_TYPE_NONE:
    emit_byte(e, TAG_NONE);
    break;
  case VM_TYPE_BOOLEAN:
    emit_byte(e, TAG_BOOL);
    emit_byte(e, obj->value.boolean ? 1 : 0);
    break;
  case VM_TYPE_INTEGER:
    emit_byte(e, TAG_INT);
    emit_i64_be(e, (int64_t)obj->value.integer);
    break;
#if VM_ENABLE_REALS
  case VM_TYPE_REAL: {
    /* Convert to network-byte-order IEEE 754 double. We do this by
       interpreting the bits of the host-order double, then byte-
       swapping if needed. Most POSIX hosts are little-endian; if the
       host is already big-endian the byte loop below is a no-op. */
    union { double d; uint8_t b[8]; } u;
    uint8_t out[8];
    int i;
    u.d = (double)obj->value.real;
    for(i = 0; i < 8; i++) {
      out[i] = u.b[7 - i];
    }
    /* Probe endianness: if a uint16_t with value 1 has its first byte
       set, host is little-endian and the swap above is correct. */
    {
      uint16_t probe = 1;
      if(*((uint8_t *)&probe) == 0) {
        /* Big-endian host: undo the swap. */
        for(i = 0; i < 8; i++) {
          out[i] = u.b[i];
        }
      }
    }
    emit_byte(e, TAG_REAL);
    emit_bytes(e, out, 8);
    break;
  }
#endif
  case VM_TYPE_CHARACTER:
    emit_byte(e, TAG_CHAR);
    emit_u32_be(e, (uint32_t)obj->value.character);
    break;
  case VM_TYPE_STRING: {
    vm_string_t *s = obj->value.string;
    const char *str = NULL;
    size_t n = 0;
    if(s != NULL) {
      if(VM_IS_SET(s->flags, VM_STRING_FLAG_ID) &&
         VM_IS_CLEAR(s->flags, VM_STRING_FLAG_RESOLVED)) {
        /* Resolve via the program's string table. */
        if(s->string_id < (vm_string_id_t)VM_TABLE_SIZE(program->strings)) {
          str = (const char *)VM_TABLE_GET(program->strings, s->string_id);
          n = VM_TABLE_LENGTH(program->strings, s->string_id);
        }
      } else {
        str = s->str;
        n = (s->length >= 0) ? (size_t)s->length
                             : (str != NULL ? strlen(str) : 0);
      }
    }
    emit_byte(e, TAG_STRING);
    if(str == NULL) {
      emit_u16_be(e, 0);
    } else {
      emit_string_payload(e, str, n);
    }
    break;
  }
  case VM_TYPE_SYMBOL: {
    vm_symbol_ref_t sref = obj->value.symbol_ref;
    const char *name = vm_symbol_lookup(program, &sref);
    emit_byte(e, TAG_SYMBOL);
    if(name == NULL) {
      emit_u16_be(e, 0);
    } else {
      emit_string_payload(e, name, strlen(name));
    }
    break;
  }
  case VM_TYPE_LIST:
    if(obj->value.list == NULL) {
      emit_byte(e, TAG_LIST);
      emit_u16_be(e, 0);
    } else {
      encode_list(e, program, obj->value.list, depth);
    }
    break;
  case VM_TYPE_VECTOR:
    if(obj->value.vector == NULL) {
      emit_byte(e, TAG_VECTOR);
      emit_u16_be(e, 0);
    } else {
      encode_vector(e, program, obj->value.vector, depth);
    }
    break;
  default: {
    /* Procedures, ports, closures, boxes, externals, forms: there's
       no useful surface representation, so emit OPAQUE with a short
       descriptor for the driver to render. */
    static const char *const names[] = {
      [VM_TYPE_PORT]      = "port",
      [VM_TYPE_PROCEDURE] = "procedure",
      [VM_TYPE_FORM]      = "form",
      [VM_TYPE_EXTERNAL]  = "external",
    };
    const char *desc = NULL;
    if(obj->type < sizeof(names) / sizeof(names[0])) {
      desc = names[obj->type];
    }
    if(desc == NULL) {
      desc = "?";
    }
    emit_byte(e, TAG_OPAQUE);
    emit_string_payload(e, desc, strlen(desc));
    break;
  }
  }
}

size_t
vm_repl_encode_obj(vm_program_t *program, const vm_obj_t *obj,
                   uint8_t *out, size_t cap)
{
  encbuf_t e;
  e.buf = out;
  e.cap = cap;
  e.pos = 0;
  e.overflow = 0;
  encode_value(&e, program, obj, 0);
  return e.overflow ? 0 : e.pos;
}

#endif /* VM_REPL_ENABLE */
