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

#include <stdio.h>
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

/*
 * GC disable counter - when > 0, garbage collection is disabled.
 * Uses a counter to support nested disable/enable calls.
 */
static int gc_disabled = 0;

/*
 * Singly-linked list of every live vm_ext_object_t allocated via
 * vm_ext_object_create. The GC walks this list after marking to call
 * each unreferenced box's type->deallocate callback, freeing the
 * type-specific opaque_data backing that the GC's heap sweep cannot
 * see otherwise.
 */
static vm_ext_object_t *ext_object_list_head = NULL;

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

  /* Pool-resident objects are tracked via the pool's ref bitmap, not the
     heap allocations hash. Cyclic structures such as a recursive closure
     capturing itself rely on this check returning true to terminate the
     mark walk. */
  if(vm_mempool_is_marked(&object_pool, ptr)) {
    return 1;
  }

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
      /* Only the heap-owned buffer needs marking. A string loaded from
         the program's string table has FLAG_ID set after resolution; its
         ->str points into program data, not into vm_alloc'd memory, and
         marking it would insert a non-heap pointer into the allocations
         table that the next sweep would then try to free. */
      if(VM_IS_SET(obj->value.string->flags, VM_STRING_FLAG_RESOLVED) &&
         VM_IS_CLEAR(obj->value.string->flags, VM_STRING_FLAG_ID)) {
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
  case VM_TYPE_EXTERNAL:
    /* The ext_object box is heap-allocated; mark it so the sweep
       phase does not free it while the parent obj is still live. */
    if(obj->value.ext_object != NULL) {
      mark_memory(obj->value.ext_object);
    }
    break;
  case VM_TYPE_BOX:
    if(obj->value.box != NULL && !memory_is_marked(obj->value.box)) {
      mark_memory(obj->value.box);
      mark_object(&obj->value.box->value);
    }
    break;
  case VM_TYPE_CLOSURE:
    if(obj->value.closure != NULL && !memory_is_marked(obj->value.closure)) {
      mark_memory(obj->value.closure);
      if(obj->value.closure->captures != NULL &&
         !memory_is_marked(obj->value.closure->captures)) {
        mark_memory(obj->value.closure->captures);
        for(k = 0; k < obj->value.closure->capture_count; k++) {
          mark_object(&obj->value.closure->captures[k]);
        }
      }
    }
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

  /* Mark the per-program captures metadata. The captures pointer array
     itself is VM_MALLOC'd and so is invisible to the GC, but each
     vm_captures_t and its symbols array come from vm_alloc and would
     otherwise be swept -- losing the symbol_ids that the closure-bind
     primitive needs to instantiate a closure. memory_is_marked makes
     this idempotent across threads that share a program. */
  if(thread->program->captures != NULL) {
    for(i = 0; i < thread->program->captures_size; i++) {
      vm_captures_t *cap = thread->program->captures[i];
      if(cap != NULL && !memory_is_marked(cap)) {
        mark_memory(cap);
        if(cap->symbols != NULL) {
          mark_memory(cap->symbols);
        }
      }
    }
  }

  mark_object(&thread->result);
  /* error.error_obj holds the most recent thrown/raised value, populated
     by vm_set_error_string / vm_set_error_object; specific_obj is the
     SRFI-18-style thread-local cell read by (thread-specific). Both can
     point at heap-allocated strings or vectors that no other root
     references. */
  mark_object(&thread->error.error_obj);
  mark_object(&thread->specific_obj);
}

void *
vm_alloc(unsigned size)
{
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
    if(!vm_hash_update(&allocations, ptr, 0)) {
      free_vm_memory(ptr);
      return NULL;
    }
    if(allocations.items > mem_stats.peak_heap_allocations) {
      mem_stats.peak_heap_allocations = allocations.items;
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
  vm_ext_object_t *box;
  vm_obj_t obj;

  /* Finalize every remaining ext-object so each type's deallocate
     callback runs before we tear the heap down underneath it. */
  while((box = ext_object_list_head) != NULL) {
    ext_object_list_head = box->next;
    if(box->type != NULL && box->type->deallocate != NULL) {
      obj.type = VM_TYPE_EXTERNAL;
      obj.value.ext_object = box;
      box->type->deallocate(&obj);
    }
  }

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
vm_gc_disable(void)
{
  gc_disabled++;
}

void
vm_gc_enable(void)
{
  if(gc_disabled > 0) {
    gc_disabled--;
  }
}

/*
 * Walk the live-ext-objects list and finalize any box that the mark
 * phase did not reach. Calling type->deallocate gives the type a chance
 * to free its opaque_data backing (mutex struct, complex struct, etc.)
 * before the upcoming heap sweep frees the box itself.
 *
 * We unlink finalized boxes from the list but do not free the box
 * memory here -- that happens in the heap sweep that runs immediately
 * after, since the box was registered in the heap allocations hash by
 * vm_alloc and is what the mark bit is keyed on.
 */
static void
finalize_unmarked_ext_objects(void)
{
  vm_ext_object_t **link;
  vm_ext_object_t *box;
  vm_obj_t obj;

  link = &ext_object_list_head;
  while((box = *link) != NULL) {
    if(memory_is_marked(box)) {
      link = &box->next;
      continue;
    }
    if(box->type != NULL && box->type->deallocate != NULL) {
      obj.type = VM_TYPE_EXTERNAL;
      obj.value.ext_object = box;
      box->type->deallocate(&obj);
    }
    *link = box->next;
  }
}

static void
do_gc(int force)
{
  unsigned i;
  unsigned deallocated;
  vm_thread_t *thread;
  void *free_ptr;

  /* Honour the disable counter and the allocation threshold unless the
     caller is forcing a sweep (e.g. for accurate live-memory reporting,
     where stale counts would be misleading). */
  if(!force) {
    if(gc_disabled > 0) {
      return;
    }
    if(allocated_since_gc < VM_GC_MIN_ALLOCATED) {
      return;
    }
  }

  mem_stats.gc_invocations++;

  /* Mark phase: mark all objects that have been allocated by the threads.
     Iterate the thread table by index, not by vm_thread_get(): the latter
     decodes a vm_id_t (which carries a nonce) and would silently return
     NULL for every loop counter, leaving everything unmarked and making a
     forced sweep free live state. */
  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    thread = vm_thread_get_by_index(i);
    if(thread != NULL) {
      mark_thread_references(thread);
    }
  }

  /* Run external-object finalizers before the sweep so each
     type->deallocate callback can free its opaque_data backing while
     the box is still valid. */
  finalize_unmarked_ext_objects();

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

  deallocated += force ? vm_mempool_gc_force(&object_pool)
                       : vm_mempool_gc(&object_pool);

  mem_stats.gc_deallocations += deallocated;

  VM_DEBUG(VM_DEBUG_HIGH, "GC: Deallocated %d of %u objects",
           deallocated,
           (unsigned)(allocations.items + object_pool.items + deallocated));

  /* Reset memory allocation counter. */
  allocated_since_gc = 0;
}

void
vm_gc(void)
{
  do_gc(0);
}

void
vm_gc_force(void)
{
  do_gc(1);
}

#if VM_ATTRIBUTION_ENABLE
void *
vm_alloc_at(unsigned size, vm_alloc_site_t site)
{
  void *ptr;
  vm_mempool_index_t index;

  ptr = vm_alloc(size);
  if(ptr != NULL && vm_mempool_is_stored(&object_pool, ptr)) {
    index = ((char *)ptr - object_pool.heap) / object_pool.obj_size;
    object_pool.alloc_sites[index] = (uint8_t)site;
  }
  return ptr;
}
#endif

vm_ext_object_t *
vm_ext_object_create(vm_obj_t *dst, vm_ext_type_t *type, void *opaque_data)
{
  vm_ext_object_t *box;

  /* Disable GC across the multi-step setup so a sweep in the middle of
     vm_alloc cannot observe an unreferenced half-built box. */
  vm_gc_disable();

  box = vm_alloc_at(sizeof(vm_ext_object_t), VM_ALLOC_SITE_EXT_OBJECT);
  if(box == NULL) {
    vm_gc_enable();
    memset(dst, 0, sizeof(vm_obj_t));
    dst->type = VM_TYPE_NONE;
    return NULL;
  }
  box->type = type;
  box->opaque_data = opaque_data;
  box->next = ext_object_list_head;
  ext_object_list_head = box;

  dst->value.ext_object = box;
  dst->type = VM_TYPE_EXTERNAL;

  vm_gc_enable();
  return box;
}

void
vm_memory_get_stats(vm_memory_stats_t *stats)
{
  memcpy(stats, &mem_stats, sizeof(vm_memory_stats_t));
}

const vm_mempool_t *
vm_object_pool(void)
{
  return &object_pool;
}

void
vm_memory_profile_print(void)
{
  vm_mempool_stats_t stats;

#if VM_ATTRIBUTION_ENABLE
  {
    /* Snapshot the per-site occupancy of the object pool BEFORE any
       force-GC runs, so the histogram reflects what was still
       resident at the moment of the print rather than the post-sweep
       residue (which is zero by construction). */
    static const char *const site_names[VM_ALLOC_SITE_COUNT] = {
      [VM_ALLOC_SITE_OTHER]           = "other",
      [VM_ALLOC_SITE_CONS_CELL]       = "cons",
      [VM_ALLOC_SITE_LIST_HEADER]     = "list_hdr",
      [VM_ALLOC_SITE_VECTOR_HEADER]   = "vec_hdr",
      [VM_ALLOC_SITE_VECTOR_ELEMENTS] = "vec_elems",
      [VM_ALLOC_SITE_VECTOR_BYTES]    = "vec_bytes",
      [VM_ALLOC_SITE_STRING_HEADER]   = "str_hdr",
      [VM_ALLOC_SITE_STRING_BUFFER]   = "str_buf",
      [VM_ALLOC_SITE_RATIONAL]        = "rational",
      [VM_ALLOC_SITE_EXT_OBJECT]      = "ext_obj",
    };
    uint32_t counts[VM_ALLOC_SITE_COUNT] = {0};
    vm_mempool_index_t i;
    unsigned byte;
    unsigned bit;
    int s;

    for(i = 0; i < object_pool.capacity; i++) {
      byte = i / (sizeof(vm_mempool_bitmap_t) * 8);
      bit = 1U << (i % (sizeof(vm_mempool_bitmap_t) * 8));
      if(VM_IS_SET(object_pool.alloc_bitmap[byte], bit)) {
        uint8_t site = object_pool.alloc_sites[i];
        if(site < VM_ALLOC_SITE_COUNT) {
          counts[site]++;
        }
      }
    }
    printf("MEM objpool by_site");
    for(s = 0; s < VM_ALLOC_SITE_COUNT; s++) {
      printf(" %s=%lu", site_names[s], (unsigned long)counts[s]);
    }
    printf("\n");
  }
#endif

#if VM_MEMORY_PROFILING_GC
  /* Force a sweep so the "used" numbers reflect live memory rather
     than live + uncollected garbage. The frame pool is manually
     lifecycled and is unaffected. */
  vm_gc_force();
#endif

  printf("MEM allocs %lu mempool_fwd %lu alloc_bytes %lu manual_deallocs %lu gc_deallocs %lu gc_invoc %lu peak_heap_allocs %lu\n",
         (unsigned long)mem_stats.allocations,
         (unsigned long)mem_stats.mempool_forwards,
         (unsigned long)mem_stats.allocated_bytes,
         (unsigned long)mem_stats.manual_deallocations,
         (unsigned long)mem_stats.gc_deallocations,
         (unsigned long)mem_stats.gc_invocations,
         (unsigned long)mem_stats.peak_heap_allocations);

  vm_mempool_get_stats(vm_object_pool(), &stats);
  printf("MEM objpool used %lu peak %lu cap %lu\n",
         (unsigned long)stats.used_bytes,
         (unsigned long)stats.peak_bytes,
         (unsigned long)stats.capacity_bytes);

  vm_mempool_get_stats(vm_frame_pool(), &stats);
  printf("MEM frmpool used %lu peak %lu cap %lu\n",
         (unsigned long)stats.used_bytes,
         (unsigned long)stats.peak_bytes,
         (unsigned long)stats.capacity_bytes);
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
