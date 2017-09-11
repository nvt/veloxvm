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

#ifndef VM_MEMPOOL_H
#define VM_MEMPOOL_H

#include <stdint.h>

typedef uint32_t vm_mempool_index_t;
typedef unsigned vm_mempool_bitmap_t;

typedef struct vm_mempool {
  vm_mempool_bitmap_t *alloc_bitmap;
  vm_mempool_bitmap_t *ref_bitmap;
  char *heap;
  vm_mempool_index_t next_free_index;
  vm_mempool_index_t items;
  vm_mempool_index_t capacity;
  uint16_t obj_size;
} vm_mempool_t;

int vm_mempool_create(vm_mempool_t *, uint16_t, vm_mempool_index_t);
void vm_mempool_destroy(vm_mempool_t *);
int vm_mempool_is_stored(vm_mempool_t *, void *);
void *vm_mempool_alloc(vm_mempool_t *);
void vm_mempool_free(vm_mempool_t *, void *);
int vm_mempool_mark(vm_mempool_t *, void *);
int vm_mempool_gc(vm_mempool_t *);

#endif /* !MEMPOOL_H */
