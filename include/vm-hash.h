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

#ifndef VM_HASH_H
#define VM_HASH_H

#include <stdint.h>

#define VM_HASH_SLOT_SET_USED(table, slot) \
  do { \
    (table)->items++; \
    (table)->used[(slot) / 8] |= 1 << ((slot) % 8); \
  } while(0)

#define VM_HASH_SLOT_SET_UNUSED(table, slot) \
  do { \
    (table)->items--; \
    (table)->used[(slot) / 8] &= ~(1 << ((slot) % 8)); \
  } while(0)

#define VM_HASH_SLOT_USED(table, slot) \
  ((table)->used[(slot) / 8] & (1 << ((slot) % 8)))

#define VM_HASH_KEYS_EQUAL(key1, key2) (key1) == (key2)

typedef uint32_t vm_hash_index_t;
typedef void * vm_hash_key_t;
typedef uint16_t vm_hash_value_t;

/*typedef int (*vm_hash_comparator_t)(vm_hash_key_t, vm_hash_key_t);*/

typedef struct vm_hash_pair {
  vm_hash_key_t key;
  vm_hash_value_t value;
} vm_hash_pair_t;

typedef struct vm_hash_table {
  vm_hash_index_t items;
  vm_hash_index_t size;
  uint8_t *used;
  vm_hash_pair_t *pairs;
} vm_hash_table_t;

#define VM_HASH_TABLE(name, size) \
  static uint8_t name_##used[(((size) * 4) / 3 + 7) / 8]; \
  static vm_hash_pair_t name_##pairs[((size) * 4) / 3]; \
  static vm_hash_table_t name = {0, ((size) * 4) / 3, name_##used, name_##pairs}

int vm_hash_update(vm_hash_table_t *, vm_hash_key_t, vm_hash_value_t);
int vm_hash_delete(vm_hash_table_t *, vm_hash_key_t);
int vm_hash_lookup(vm_hash_table_t *, vm_hash_key_t, vm_hash_value_t *);

#endif /* !VM_HASH_H */
