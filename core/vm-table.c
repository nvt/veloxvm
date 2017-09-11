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

#include <stdlib.h>

int
vm_table_create(vm_table_t *table, unsigned item_count, unsigned table_size)
{
  if(item_count == 0) {
    /* We allow empty tables to be created. */
    table->item_count = 0;
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
  if(table->item_count > 0) {
    VM_FREE(table->raw_table);
    VM_FREE(table->items);
    VM_FREE(table->item_lengths);
  }
}

int
vm_table_set(vm_table_t *table, unsigned index, void *ptr, unsigned item_length)
{
  table->items[index] = table->next_free;
  memcpy(table->items[index], ptr, item_length);
  table->items[index][item_length] = '\0';
  table->item_lengths[index] = item_length;
  table->next_free += item_length + 1;

  return 1;
}
