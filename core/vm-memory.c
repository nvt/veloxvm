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

/*
 * This is a mark-and-sweep garbage collector for dynamically
 * allocated memory in the VM.
 *
 * The garbage collection occurs on a per-thread basis, whenever the
 * flag VM_THREAD_FLAG_GC is set and the scheduler is about to execute
 * a new expression.
 *
 * The allocations and deallocations are made through the port-specific
 * VM_ALLOC and VM_FREE macros. These are typically defined to use the
 * heap allocator provided by the system environment.
 *
 * The garbage collector will go through three distinct areas to find
 * memory addresses to mark:
 *
 * 1) Objects currently residing on the expression stack of the thread.
 * 2) Lexically bound objects.
 * 3) Dynamically bound objects.
 *
 * Once the marking phase is completed, the garbage collector sweeps over
 * the active allocation set, and deallocates any object that is not marked.
 */

#include <string.h>

#include "vm.h"
#include "vm-hash.h"
#include "vm-log.h"
#include "vm-mempool.h"

#define VM_POOL_ELEMENT_SIZE    sizeof(vm_list_item_t)
#define VM_MAX_POOL_ALLOCATIONS (VM_OBJECT_POOL_SIZE / VM_POOL_ELEMENT_SIZE)
#define VM_MAX_HEAP_ALLOCATIONS (VM_HEAP_SIZE / VM_POOL_ELEMENT_SIZE)

/*
 * This hash table holds references to all current memory
 * allocations. It is used by the mark-and-sweep GC algorithm to mark
 * allocations that are referenced by VM threads. The key of the hash
 * table is the address of the object, whilst the value is a boolean
 * indicating whether the object is currently referenced by any VM
 * thread. The GC algorithm * clears the value after each iteration.
 */
VM_HASH_TABLE(allocations, VM_MAX_HEAP_ALLOCATIONS);

/*
 * Smaller allocations are placed in an object pool, which has a lower
 * overhead per allocation, is faster to handle allocations and deallocations,
 * and ameliorates fragmentation problems in the main heap.
 */
static vm_mempool_t object_pool;
static unsigned allocated_since_gc;
static vm_memory_stats_t mem_stats;

static void
free_vm_memory(void *ptr)
{
  if(vm_mempool_is_stored(&object_pool, ptr)) {
    vm_mempool_free(&object_pool, ptr);
  } else {
    VM_FREE(ptr);
  }
}

static int
memory_is_marked(void *ptr)
{
  int r;
  vm_hash_value_t value;

  r = vm_hash_lookup(&allocations, ptr, &value);
  return r && value > 0;
}

static void
mark_memory(void *ptr)
{
  if(!vm_mempool_mark(&object_pool, ptr)) {
    /*
     * If the memory pool could not mark the object, it has been
     * allocated through the heap allocator and the references are
     * stored in the hash table.
     */
    if(vm_hash_update(&allocations, ptr, 1)) {
      VM_DEBUG(VM_DEBUG_HIGH, "GC: Mark pointer %p", ptr);
    } else {
      VM_DEBUG(VM_DEBUG_HIGH, "GC: Attempting to mark unknown pointer %p!",
	       ptr);
    }
  }
}

static void
mark_object(vm_obj_t *obj)
{
  int k;
  vm_list_item_t *item;

  /* We need to mark only the object types that involve heap memory. */
  switch(obj->type) {
  case VM_TYPE_RATIONAL:
     mark_memory(obj->value.rational);
     break;
  case VM_TYPE_LIST:
    if(obj->value.list != NULL && !memory_is_marked(obj->value.list)) {
      mark_memory(obj->value.list);
      /* TODO: Avoid recursion. */
      for(item = obj->value.list->head; item != NULL; item = item->next) {
        mark_memory(item);
        mark_object(&item->obj);
      }
    }
    break;
  case VM_TYPE_PORT:
    if(!memory_is_marked(obj->value.port)) {
      mark_memory(obj->value.port);
    }
    break;
  case VM_TYPE_STRING:
    if(!memory_is_marked(obj->value.string)) {
      if(IS_SET(obj->value.string->flags, VM_STRING_FLAG_RESOLVED)) {
        mark_memory(obj->value.string->str);
      }
      mark_memory(obj->value.string);
    }
    break;
  case VM_TYPE_VECTOR:
    if(obj->value.vector->bytes != NULL &&
       !memory_is_marked(obj->value.vector->bytes)) {
      mark_memory(obj->value.vector->bytes);
    } else if(obj->value.vector->elements != NULL &&
              !memory_is_marked(obj->value.vector->elements)) {
      mark_memory(obj->value.vector->elements);
      for(k = 0; k < obj->value.vector->length; k++) {
        mark_object(&obj->value.vector->elements[k]);
      }
    }
    mark_memory(obj->value.vector);
    break;
  default:
    break;
  }
}

static void
mark_thread_references(vm_thread_t *thread)
{
  unsigned i;
  unsigned j;
  vm_expr_t *expr;

  /* Pass over all objects referenced by a certain thread. */
  for(i = 0; i < thread->exprc; i++) {
    expr = thread->exprv[i];

    for(j = 0; j < expr->argc; j++) {
      mark_object(&expr->argv[j]);
    }

    for(j = 0; j < expr->bindc; j++) {
      VM_DEBUG(VM_DEBUG_HIGH, "GC: Mark bind %d,%d", i, j);
      mark_object(&expr->bindv[j].obj);
    }
  }

  for(i = 0; i < VM_TABLE_SIZE(thread->program->symbols); i++) {
    mark_object(&thread->program->symbol_bindings[i]);
  }

  mark_object(&thread->result);
}

void *
vm_alloc(unsigned size)
{
  static unsigned sum;
  vm_thread_t *thread;
  int put_in_hash;
  void *ptr;

  thread = vm_current_thread();
  put_in_hash = 1;

  if(size <= VM_POOL_ELEMENT_SIZE) {
    size = VM_POOL_ELEMENT_SIZE;
    ptr = vm_mempool_alloc(&object_pool);
    if(ptr == NULL) {
      /* Try to allocate memory in the regular heap if
         the memory pool is full. */
      vm_gc();
      ptr = VM_MALLOC(size);
    } else {
      /* Don't insert the allocated object into the hash table because
         the memory pool handles garbage collection by itself. */
      put_in_hash = 0;
      mem_stats.mempool_forwards++;
    }
  } else {
    ptr = VM_MALLOC(size);
  }

  if(ptr == NULL) {
    /* The allocation failed; try to run the garbage collector and then
       make another attempt at allocating the object on the heap. */
    vm_gc();
    ptr = VM_MALLOC(size);
    if(ptr == NULL) {
      if(thread != NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
      }
      return NULL;
    }
  }

  if(put_in_hash) {
    sum += size;
    if(!vm_hash_update(&allocations, ptr, 0)) {
      free_vm_memory(ptr);
      return NULL;
    }
  }

  VM_DEBUG(VM_DEBUG_HIGH, "GC: Alloc ptr %p, size %d", ptr, (int)size);

  /* Notify the scheduler that this thread is eligible
     for garbage collection. */
  if(thread != NULL) {
    allocated_since_gc += size;
    thread->stats.allocated_total += size;
  }

  mem_stats.allocations++;
  mem_stats.allocated_bytes += size;

  return ptr;
}

void
vm_free(void *ptr)
{
  VM_DEBUG(VM_DEBUG_HIGH, "GC: Free ptr %p", ptr);

  if(ptr == NULL ||
     (!vm_mempool_is_stored(&object_pool, ptr) &&
      !vm_hash_delete(&allocations, ptr))) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "GC: Attempt to deallocate unknown memory! (%p)",
             ptr);
  } else {
    free_vm_memory(ptr);
    mem_stats.manual_deallocations++;
  }
}

void
vm_free_all(void)
{
  unsigned i;
  unsigned deallocated;

  for(i = deallocated = 0; i < allocations.size; i++) {
    if(VM_HASH_SLOT_USED(&allocations, i)) {
      free_vm_memory(allocations.pairs[i].key);
      VM_HASH_SLOT_SET_UNUSED(&allocations, i);
      deallocated++;
    }
  }

  deallocated += object_pool.items;
  vm_mempool_destroy(&object_pool);
  mem_stats.manual_deallocations += deallocated;
  VM_DEBUG(VM_DEBUG_HIGH,
           "GC: Deallocated the remaining %u objects", deallocated);
}

void
vm_gc(void)
{
  unsigned i;
  unsigned deallocated;
  vm_thread_t *thread;
  void *free_ptr;

  if(allocated_since_gc < VM_GC_MIN_ALLOCATED) {
    /* The loaded programs have not yet allocated enough memory for
       the garbage collection algorithm to run on it. */
    return;
  }

  mem_stats.gc_invocations++;

  /* Mark phase: mark all objects that have been allocated by the threads. */
  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    thread = vm_thread_get(i);
    if(thread != NULL) {
      mark_thread_references(thread);
    }
  }

  /* Sweep phase: deallocate all unreferenced objects allocated on the heap. */
  for(i = deallocated = 0; i < allocations.size; i++) {
    if(VM_HASH_SLOT_USED(&allocations, i) && allocations.pairs[i].value == 0) {
      /* Free the memory if it has been allocated and stored in the hash
         table, but no references to it could be found. */
      free_ptr = allocations.pairs[i].key;
      VM_DEBUG(VM_DEBUG_HIGH, "GC: Free allocation %d, address %p",
               i, free_ptr);
      VM_HASH_SLOT_SET_UNUSED(&allocations, i);
      deallocated++;
      free_vm_memory(free_ptr);
    }

    /* Clear the value for the next invocation of the GC algorithm
       before moving on to the next object to process. */
    allocations.pairs[i].value = 0;
  }

  deallocated += vm_mempool_gc(&object_pool);

  mem_stats.gc_deallocations += deallocated;

  VM_DEBUG(VM_DEBUG_HIGH, "GC: Deallocated %d of %u objects",
           deallocated,
           (unsigned)(allocations.items + object_pool.items + deallocated));

  /* Reset memory allocation counter. */
  allocated_since_gc = 0;
}

void
vm_memory_get_stats(vm_memory_stats_t *stats)
{
  memcpy(stats, &mem_stats, sizeof(vm_memory_stats_t));
}

int
vm_memory_init(void)
{
  VM_DEBUG(VM_DEBUG_MEDIUM,
           "Heap size %u, object pool size %u, pool element size %u",
           VM_HEAP_SIZE, VM_OBJECT_POOL_SIZE, VM_POOL_ELEMENT_SIZE);
  VM_DEBUG(VM_DEBUG_MEDIUM,
           "Max heap allocations: %u, max pool allocations %u",
           VM_MAX_HEAP_ALLOCATIONS, VM_MAX_POOL_ALLOCATIONS);

  return vm_mempool_create(&object_pool, VM_POOL_ELEMENT_SIZE,
                           VM_MAX_POOL_ALLOCATIONS);
}
