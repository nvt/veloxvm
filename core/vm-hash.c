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

#include <string.h>

#include "vm.h"
#include "vm-hash.h"
#include "vm-log.h"

/* If more than 3/4 of all slots are used, we consider the hash
   table to be full. */
#define VM_HASH_IS_FULL(table) \
  ((table)->items * 4 > (table)->size * 3)

/*
 * A hash table implementation that uses double hashing, and linear
 * probing on a limited number of slots after the index obtained by
 * the second hash function.
 */

#define VM_HASH_LINEAR_PROBING_LIMIT 50

static vm_hash_index_t
calculate_hash(vm_hash_key_t key)
{
  vm_hash_index_t hash;
  unsigned i;

  /* Bernstein's hash function. */
  hash = 0;
  for(i = 0; i < sizeof(key); i++) {
    hash += hash * 33 + (((intptr_t)key >> i) & 0xff);
  }

  return hash;
}

static vm_hash_index_t
calculate_hash2(vm_hash_key_t key)
{
  vm_hash_index_t hash;
  unsigned i;

  /* Jenkin's hash function. */
  hash = 0;
  for(i = 0; i < sizeof(key); i++) {
    hash += ((intptr_t)key >> i) & 0xff;
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  return hash ^ (intptr_t)key;
}

static int
get_index(vm_hash_table_t *table, vm_hash_key_t key, vm_hash_index_t *index)
{
  vm_hash_index_t tmp_index;
  int first_slot_used;
  int i;

  *index = calculate_hash(key) % table->size;
  first_slot_used = VM_HASH_SLOT_USED(table, *index);
  if(first_slot_used && VM_HASH_KEYS_EQUAL(table->pairs[*index].key, key)) {
    /* We found the key after the first hash. */
    return 1;
  }

  /* We haven't found the key after the first hash, so try to find it in
     a slot obtained from the second hash function. */
  tmp_index = (*index + calculate_hash2(key)) % table->size;
  for(i = 0; i < VM_HASH_LINEAR_PROBING_LIMIT; i++, tmp_index++) {
    if(!VM_HASH_SLOT_USED(table, tmp_index)) {
      /* The key was not found, so set the index to the first empty slot
	 of the pair of slots obtained with the two hash functionsa. */
      if(first_slot_used) {
	*index = tmp_index;
      }
      return 1;
    } else if(VM_HASH_KEYS_EQUAL(table->pairs[tmp_index].key, key)) {
      /* The key was found by using linear probing. */
      *index = tmp_index;
      return 1;
    }
  }

  /*
   * At this point, where neither a free slot nor a matching key could
   * be found by using linear probing, the success of the operation
   * depends on whether the first slot is free.
   */
  return !first_slot_used;
}

int
vm_hash_update(vm_hash_table_t *table, vm_hash_key_t key, vm_hash_value_t value)
{
  vm_hash_index_t index;

  if(!get_index(table, key, &index)) {
    VM_DEBUG(VM_DEBUG_LOW,
           "A hash collision occured for key %p! Items = %lu",
	   key, (unsigned long)table->items);
    return 0;
  }

  /* Determine whether we need to allocate a new slot. */
  if(!VM_HASH_SLOT_USED(table, index)) {
    if(VM_HASH_IS_FULL(table)) {
      VM_DEBUG(VM_DEBUG_LOW, "A hash table is full! Items = %lu",
	     (unsigned long)table->items);
      return 0;
    }

    VM_HASH_SLOT_SET_USED(table, index);
  }

  table->pairs[index].key = key;
  table->pairs[index].value = value;
  return 1;
}

int
vm_hash_delete(vm_hash_table_t *table, vm_hash_key_t key)
{
  vm_hash_index_t index;

  if(!get_index(table, key, &index)) {
    return 0;
  }

  VM_HASH_SLOT_SET_UNUSED(table, index);
  return 1;
}

int
vm_hash_lookup(vm_hash_table_t *table, vm_hash_key_t key,
               vm_hash_value_t *value)
{
  vm_hash_index_t index;

  if(!get_index(table, key, &index) || !VM_HASH_SLOT_USED(table, index)) {
    return 0;
  }

  memcpy(value, &table->pairs[index].value, sizeof(vm_hash_value_t));
  return 1;
}
