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
#include "vm-table.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int
vm_table_create(vm_table_t *table, unsigned item_count, uint32_t table_size)
{
  if(table_size > VM_TABLE_MAX_BYTES) {
    VM_DEBUG(VM_DEBUG_LOW, "Attempt to create a too large table %lu",
             (unsigned long)table_size);
    return 0;
  }

#ifdef VM_REPL_ENABLE
  table->arena_item_count = item_count;
  table->items_capacity = item_count;
#endif

  if(item_count == 0) {
    /* We allow empty tables to be created. */
    table->item_count = 0;
    table->available_size = 0;
    table->raw_table = NULL;
    table->items = NULL;
    table->item_lengths = NULL;
    return 1;
  }

  table->raw_table = VM_MALLOC(table_size);
  if(table->raw_table == NULL) {
    return 0;
  }
  table->size = table_size;
  table->available_size = table_size;
  table->next_free = table->raw_table;

  table->items = VM_MALLOC(sizeof(*table->items) * item_count);
  if(table->items == NULL) {
    VM_FREE(table->raw_table);
    return 0;
  }

  table->item_lengths = VM_MALLOC(sizeof(*table->item_lengths) * item_count);
  if(table->item_lengths == NULL) {
    VM_FREE(table->raw_table);
    VM_FREE(table->items);
    return 0;
  }

  table->item_count = item_count;

  return 1;
}

void
vm_table_destroy(vm_table_t *table)
{
#ifdef VM_REPL_ENABLE
  /* Free items appended outside the arena. arena_item_count equals
     item_count for tables that never grew, so this loop is a no-op
     in that case. */
  if(table->items != NULL) {
    unsigned i;
    for(i = table->arena_item_count; i < table->item_count; i++) {
      VM_FREE(table->items[i]);
    }
  }
#endif

  if(table->item_count > 0
#ifdef VM_REPL_ENABLE
     || table->items_capacity > 0
#endif
     ) {
    VM_FREE(table->raw_table);
    VM_FREE(table->items);
    VM_FREE(table->item_lengths);
  }
}

int
vm_table_set(vm_table_t *table, unsigned index, void *ptr, unsigned item_length)
{
  if(index >= table->item_count || item_length + 1 > table->available_size) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to insert a table item");
    return 0;
  }

  table->items[index] = table->next_free;
  memcpy(table->items[index], ptr, item_length);
  table->items[index][item_length] = '\0';
  table->item_lengths[index] = item_length;
  table->next_free += item_length + 1;
  table->available_size -= item_length + 1;

  return 1;
}

#ifdef VM_REPL_ENABLE

int
vm_table_init_growable(vm_table_t *table)
{
  table->raw_table = NULL;
  table->size = 0;
  table->available_size = 0;
  table->next_free = NULL;
  table->items = NULL;
  table->item_lengths = NULL;
  table->item_count = 0;
  table->arena_item_count = 0;
  table->items_capacity = 0;
  return 1;
}

static int
grow_index_arrays(vm_table_t *table, unsigned needed)
{
  unsigned new_cap;
  uint8_t **new_items;
  uint16_t *new_lengths;

  if(needed <= table->items_capacity) {
    return 1;
  }

  new_cap = table->items_capacity == 0 ? 8 : table->items_capacity;
  while(new_cap < needed) {
    if(new_cap > UINT32_MAX / 2) {
      return 0;
    }
    new_cap *= 2;
  }

  new_items = VM_REALLOC(table->items, sizeof(*new_items) * new_cap);
  if(new_items == NULL) {
    return 0;
  }
  table->items = new_items;

  new_lengths = VM_REALLOC(table->item_lengths,
                           sizeof(*new_lengths) * new_cap);
  if(new_lengths == NULL) {
    return 0;
  }
  table->item_lengths = new_lengths;

  table->items_capacity = new_cap;
  return 1;
}

int
vm_table_append(vm_table_t *table, const void *bytes, unsigned len)
{
  uint8_t *copy;
  unsigned index;

  if(len > UINT16_MAX) {
    VM_DEBUG(VM_DEBUG_LOW, "vm_table_append: item too large (%u)", len);
    return -1;
  }

  if(grow_index_arrays(table, table->item_count + 1) == 0) {
    return -1;
  }

  copy = VM_MALLOC(len + 1);
  if(copy == NULL) {
    return -1;
  }
  if(len > 0) {
    memcpy(copy, bytes, len);
  }
  copy[len] = '\0';

  index = table->item_count;
  table->items[index] = copy;
  table->item_lengths[index] = (uint16_t)len;
  table->item_count = index + 1;
  return (int)index;
}

#endif /* VM_REPL_ENABLE */
