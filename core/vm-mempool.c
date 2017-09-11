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
 * 3. Neither the name of the author nor the names of the contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm-log.h"
#include "vm-mempool.h"

#ifdef VM_MEMPOOL_CONF_ALLOC
#define VM_MEMPOOL_ALLOC VM_MEMPOOL_CONF_ALLOC
#else
#define VM_MEMPOOL_ALLOC malloc
#endif

#ifdef VM_MEMPOOL_CONF_FREE
#define VM_MEMPOOL_FREE VM_MEMPOOL_CONF_FREE
#else
#define VM_MEMPOOL_FREE free
#endif

#define BITNUM (sizeof(vm_mempool_bitmap_t) * CHAR_BIT)
#define BITMAP_SIZE(capacity) \
  (((capacity) + BITNUM - 1) / BITNUM)
#define HEAP_END(pool) \
  ((char *)(pool)->heap + \
   (pool)->obj_size * (pool)->capacity)

static void
update_next_free_index(vm_mempool_t *pool)
{
  vm_mempool_index_t index;
  unsigned byte;
  unsigned bit;

  if(pool->items == pool->capacity) {
    /*
     * No free slot exists. Set the next-free index to the highest
     * index. The next-free index will be updated the next time an
     * object is deallocated.
     */
    pool->next_free_index = pool->capacity;
    return;
  }

  index = pool->next_free_index;
  do {
    if(++index == pool->capacity) {
      index = 0;
    }
    byte = index / BITNUM;
    bit = 1U << (index % BITNUM);

    if(pool->alloc_bitmap[byte] != ~((vm_mempool_bitmap_t)0)) {
      if(IS_CLEAR(pool->alloc_bitmap[byte], bit)) {
        pool->next_free_index = index;
        return;
      }
    }
  } while(index != pool->next_free_index);

  VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to update mempool index");
}

int
vm_mempool_create(vm_mempool_t *pool, uint16_t obj_size,
                  vm_mempool_index_t capacity)
{
  size_t bitmap_size;

  pool->obj_size = obj_size;
  pool->capacity = capacity;
  pool->items = 0;
  pool->next_free_index = 0;

  bitmap_size = BITMAP_SIZE(capacity) * sizeof(vm_mempool_bitmap_t);

  pool->alloc_bitmap = VM_MEMPOOL_ALLOC(bitmap_size);
  if(pool->alloc_bitmap == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM,
             "Failed to create a mempool alloc bitmap of size %lu\n",
             (unsigned long)bitmap_size);
    return 0;
  }
  pool->ref_bitmap = VM_MEMPOOL_ALLOC(bitmap_size);
  if(pool->ref_bitmap == NULL) {
    VM_MEMPOOL_FREE(pool->alloc_bitmap);
    VM_DEBUG(VM_DEBUG_MEDIUM,
             "Failed to create a mempool ref bitmap of size %lu\n",
             (unsigned long)bitmap_size);
    return 0;
  }
  memset(pool->alloc_bitmap, 0, bitmap_size);
  memset(pool->ref_bitmap, 0, bitmap_size);

  pool->heap = VM_MEMPOOL_ALLOC(obj_size * capacity);
  if(pool->heap == 0) {
    VM_MEMPOOL_FREE(pool->alloc_bitmap);
    VM_MEMPOOL_FREE(pool->ref_bitmap);
    VM_DEBUG(VM_DEBUG_MEDIUM,
             "Failed to create a mempool heap of size %lu\n",
             (unsigned long)(obj_size * capacity));
    return 0;
  }

  return 1;
}

void
vm_mempool_destroy(vm_mempool_t *pool)
{
  VM_MEMPOOL_FREE(pool->alloc_bitmap);
  VM_MEMPOOL_FREE(pool->ref_bitmap);
  VM_MEMPOOL_FREE(pool->heap);
  pool->items = 0;
  pool->capacity = 0;
}

int
vm_mempool_is_stored(vm_mempool_t *pool, void *obj)
{
  return (char *)obj >= pool->heap &&
         (char *)obj < HEAP_END(pool);
}

void *
vm_mempool_alloc(vm_mempool_t *pool)
{
  vm_mempool_index_t index;
  unsigned byte;
  unsigned bit;

  index = pool->next_free_index;
  byte = index / BITNUM;
  bit = 1U << (index % BITNUM);

  if(pool->items >= pool->capacity ||
     IS_SET(pool->alloc_bitmap[byte], bit)) {
    return NULL;
  }

  pool->items++;
  update_next_free_index(pool);

  SET(pool->alloc_bitmap[byte], bit);
  return (char *)pool->heap + index * pool->obj_size;
}

void
vm_mempool_free(vm_mempool_t *pool, void *obj)
{
  vm_mempool_index_t index;
  unsigned byte;
  unsigned bit;

  if(vm_mempool_is_stored(pool, obj)) {
    index = ((char *)obj - (char *)pool->heap) / pool->obj_size;
    byte = index / BITNUM;
    bit = 1U << (index % BITNUM);

    if(IS_CLEAR(pool->alloc_bitmap[byte], bit)) {
      VM_DEBUG(VM_DEBUG_LOW, "Freeing already free object %p!", obj);
    }

    CLEAR(pool->alloc_bitmap[byte], bit);
    CLEAR(pool->ref_bitmap[byte], bit);
    if(index < pool->next_free_index) {
      pool->next_free_index = index;
    }
    pool->items--;
  }
}

int
vm_mempool_mark(vm_mempool_t *pool, void *obj)
{
  vm_mempool_index_t index;
  unsigned byte;
  unsigned bit;

  if(!vm_mempool_is_stored(pool, obj)) {
    return 0;
  }

  index = ((char *)obj - (char *)pool->heap) / pool->obj_size;
  byte = index / BITNUM;
  bit = 1U << (index % BITNUM);

  SET(pool->ref_bitmap[byte], bit);

  return 1;
}

int
vm_mempool_gc(vm_mempool_t *pool)
{
  vm_mempool_index_t index;
  unsigned byte;
  unsigned bit;
  int released_objects;

  /* Avoid scanning the memory pool if the number of allocated objects
     is less than two thirds of the capacity. */
  if(pool->items < (2 * pool->capacity) / 3) {
    return 0;
  }

  released_objects = 0;

  for(index = 0; index < pool->capacity; index++) {
    byte = index / BITNUM;
    bit = 1U << (index % BITNUM);

    if(IS_SET(pool->alloc_bitmap[byte], bit) &&
       IS_CLEAR(pool->ref_bitmap[byte], bit)) {
      CLEAR(pool->alloc_bitmap[byte], bit);
      pool->items--;
      if(index < pool->next_free_index) {
	pool->next_free_index = index;
      }
      released_objects++;
    }
    CLEAR(pool->ref_bitmap[byte], bit);
  }

  return released_objects;
}
