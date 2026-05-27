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
#include "vm-log.h"
#include "vm-mempool.h"

#if VM_PAUSE_PROFILING
#include "vm-port.h"

/* Convert a vm_native_time_t reading to nanoseconds. POSIX returns
   ns directly (resolution = 1e9 ticks/s), Contiki-NG returns rtimer
   ticks at RTIMER_SECOND resolution. Histogram bucketing is in
   log2 microseconds so per-port precision differences are absorbed
   by the bucket width. */
static uint64_t
pause_now_ns(void)
{
  return (uint64_t)VM_NATIVE_TIME() *
         (1000000000ULL / VM_NATIVE_TIME_RESOLUTION());
}

static unsigned
pause_bucket(uint64_t ns)
{
  uint64_t us;
  unsigned b;

  if(ns < 1000) {
    return 0;
  }
  us = ns / 1000;
  b = 0;
  while(us > 0 && b < VM_PAUSE_BUCKETS - 1) {
    us >>= 1;
    b++;
  }
  return b;
}
#endif

/* Pool slot size must accommodate every type that the small-allocation
   path forwards through it. vm_pair_t (32 B on 64-bit) is the largest;
   vm_list_item_t (24 B) and smaller types pay padding per slot. */
/* Set to 1 to check every marked pointer against the allocation list.
   O(live) per mark, so it is a debugging aid rather than a build
   default; see the ownership contract on memory_is_marked. */
#ifndef VM_GC_VALIDATE
#define VM_GC_VALIDATE 0
#endif

#define VM_POOL_ELEMENT_SIZE    sizeof(vm_pair_t)
#define VM_MAX_POOL_ALLOCATIONS (VM_OBJECT_POOL_SIZE / VM_POOL_ELEMENT_SIZE)

/*
 * Every heap-tier allocation carries a header holding its mark bit, its
 * payload size, and its links in the list of live heap allocations.
 * Marking is thus a store rather than a lookup in a pointer-keyed hash
 * table, freeing is an unlink, and the sweep walks the allocations that
 * exist rather than every slot of a table sized for the worst case.
 */
typedef struct vm_heap_header {
  struct vm_heap_header *next;
  struct vm_heap_header *prev;
  uint32_t size;
  uint8_t marked;
} vm_heap_header_t;

/*
 * The payload begins immediately after the header, so the header size
 * decides the payload alignment. Rounding that size up through a union
 * with the strictest scalar types keeps the payload aligned for every
 * type the VM stores there, including vm_obj_t.
 */
typedef union vm_heap_cell {
  vm_heap_header_t header;
  void *align_ptr;
  double align_double;
  long align_long;
} vm_heap_cell_t;

#define VM_HEAP_HEADER_SIZE sizeof(vm_heap_cell_t)

static vm_heap_header_t *heap_list_head;
static unsigned heap_live_items;
static size_t heap_live_bytes;

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

/*
 * Singly-linked list of every heap-allocated vm_port_t registered via
 * vm_port_register. The GC walks this list before the heap sweep and
 * calls io->close on any unmarked port that is still flagged OPEN, so
 * the underlying fd / opaque_desc is released before the heap sweep
 * frees the port struct itself. Static port singletons (stdin/stdout
 * in vm-native.c) are not on the list.
 */
static vm_port_t *port_list_head = NULL;

static vm_heap_header_t *
heap_header(void *ptr)
{
  return (vm_heap_header_t *)((char *)ptr - VM_HEAP_HEADER_SIZE);
}

static void
heap_unlink(vm_heap_header_t *header)
{
  if(header->prev == NULL) {
    heap_list_head = header->next;
  } else {
    header->prev->next = header->next;
  }
  if(header->next != NULL) {
    header->next->prev = header->prev;
  }
}

#if VM_GC_VALIDATE
/*
 * Walk the allocation list looking for ptr. Only used to assert the
 * ownership contract below; O(live) per call, so it is compiled out
 * unless VM_GC_VALIDATE is set.
 */
static int
heap_owns(void *ptr)
{
  vm_heap_header_t *header;

  for(header = heap_list_head; header != NULL; header = header->next) {
    if((char *)header + VM_HEAP_HEADER_SIZE == (char *)ptr) {
      return 1;
    }
  }
  return 0;
}

static void
assert_gc_owned(void *ptr)
{
  if(!vm_mempool_is_stored(&object_pool, ptr) && !heap_owns(ptr)) {
    VM_DEBUG(VM_DEBUG_LOW,
             "GC: mark of pointer %p that the GC does not own", ptr);
    VM_PRINTF("GC VALIDATION: unowned pointer %p reached the mark phase\n",
              ptr);
  }
}
#else
#define assert_gc_owned(ptr)
#endif

static void
free_vm_memory(void *ptr)
{
  vm_heap_header_t *header;

  if(vm_mempool_is_stored(&object_pool, ptr)) {
    vm_mempool_free(&object_pool, ptr);
    return;
  }

  header = heap_header(ptr);
  heap_unlink(header);
  heap_live_items--;
  heap_live_bytes -= header->size;
  VM_FREE(header);
}

/*
 * Both of the following require a pointer the GC owns: pool-resident,
 * or returned by the heap path of vm_alloc. For anything else they
 * would read a header in front of an object the allocator never handed
 * out, so the mark walk must not reach a pointer the VM did not
 * allocate. Statically allocated ports are the case to watch, and are
 * kept out by marking a port only when VM_PORT_FLAG_HEAP says
 * vm_port_register accepted it. Building with VM_GC_VALIDATE checks
 * every mark against the allocation list.
 */
static int
memory_is_marked(void *ptr)
{
  /* A constructor that fails partway leaves its destination tagged
     (STRING, VECTOR, CLOSURE) with a NULL payload, so the marker can be
     handed one. NULL has no header to read, and there is nothing to
     descend into or to free, so report it as already handled. */
  if(ptr == NULL) {
    return 1;
  }

  /* Pool-resident objects are tracked via the pool's ref bitmap. Cyclic
     structures such as a recursive closure capturing itself rely on this
     check returning true to terminate the mark walk. */
  if(vm_mempool_is_marked(&object_pool, ptr)) {
    return 1;
  }
  if(vm_mempool_is_stored(&object_pool, ptr)) {
    return 0;
  }

  assert_gc_owned(ptr);
  return heap_header(ptr)->marked;
}

static void
mark_memory(void *ptr)
{
  if(ptr == NULL) {
    return;
  }

  if(vm_mempool_mark(&object_pool, ptr)) {
    return;
  }

  assert_gc_owned(ptr);
  heap_header(ptr)->marked = 1;
  VM_DEBUG(VM_DEBUG_HIGH, "GC: Mark pointer %p", ptr);
}

/*
 * The mark phase uses an explicit work list rather than the C call
 * stack, for the same reason that bytecode execution keeps its own
 * expression-frame stack: thread depth should be bounded by
 * VM_CONTEXT_STACK_SIZE, not by a native stack that is a couple of
 * kilobytes on the smaller targets.
 *
 * Entries are vm_obj_t pointers into live containers: a pair's car, a
 * vector element, a capture slot. Duplicates are allowed, since the pop
 * handler applies the memory_is_marked guard, so each object is expanded
 * once and cycles terminate there.
 */
static vm_obj_t *mark_stack[VM_MARK_STACK_SIZE];
static unsigned mark_top;

static void mark_expand(vm_obj_t *obj);

/*
 * Scalars (integers, characters, booleans, nil, symbols carried by ID)
 * own no heap memory and have nothing to descend into. Filtering them
 * out before they reach the work list is what keeps it shallow in
 * practice: a vector of ten thousand integers pushes nothing.
 */
static int
is_heap_bearing(vm_obj_type_t type)
{
  switch(type) {
  case VM_TYPE_PAIR:
  case VM_TYPE_VECTOR:
  case VM_TYPE_BOX:
  case VM_TYPE_CLOSURE:
  case VM_TYPE_STRING:
  case VM_TYPE_PORT:
  case VM_TYPE_RATIONAL:
  case VM_TYPE_EXTERNAL:
    return 1;
  default:
    return 0;
  }
}

static void
mark_push(vm_obj_t *obj)
{
  if(obj == NULL || !is_heap_bearing(obj->type)) {
    return;
  }

  if(mark_top == VM_MARK_STACK_SIZE) {
    /* The work list is saturated. Expand this reference in place rather
       than drop it, since dropping one leaves a reachable object
       unmarked for the sweep to free. Expanding in place recurses, so
       a structure wide enough to saturate the list is marked on the
       native stack after all, but only along the branch that overflowed.
       mark_stack_peak and mark_overflows in the memory stats report when
       that happens and what to size VM_MARK_STACK_SIZE against. */
    mem_stats.mark_overflows++;
    mark_expand(obj);
    return;
  }

  mark_stack[mark_top++] = obj;
  if(mark_top > mem_stats.mark_stack_peak) {
    mem_stats.mark_stack_peak = mark_top;
  }
}

static void
mark_expand(vm_obj_t *obj)
{
  vm_pair_t *pair;
  int k;


  /* We need to mark only the object types that involve heap memory. */
  switch(obj->type) {
  case VM_TYPE_RATIONAL:
     mark_memory(obj->value.rational);
     break;
  case VM_TYPE_PAIR:
    /* Walk the cdr spine with a loop rather than a push per link. A
       proper list is a right-nested chain, so iterating it costs one
       work-list slot however long the list is; only the car of each
       link, and a non-pair (improper) tail, become work items. The
       empty list is VM_TYPE_PAIR with a NULL payload, which ends the
       loop along with the already-marked check that breaks cycles. */
    for(pair = obj->value.pair;
        pair != NULL && !memory_is_marked(pair);
        pair = pair->cdr.type == VM_TYPE_PAIR ? pair->cdr.value.pair : NULL) {
      mark_memory(pair);
      mark_push(&pair->car);
      if(pair->cdr.type != VM_TYPE_PAIR) {
        mark_push(&pair->cdr);
      }
    }
    break;
  case VM_TYPE_PORT:
    /* Only heap-allocated ports carry a GC header. The native port's
       stdin/stdout singletons are static and never registered, so
       reading a header in front of them would address whatever the
       linker placed there. */
    if(obj->value.port != NULL && obj->value.port->heap_allocated &&
       !memory_is_marked(obj->value.port)) {
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
    /* vm_vector_create tags obj before it can know that the allocation
       succeeded, so a construction that failed partway reaches the
       marker as a vector with a NULL header. Without this check, the
       reads below would dereference it. */
    if(obj->value.vector == NULL) {
      break;
    }
    if(obj->value.vector->bytes != NULL &&
       !memory_is_marked(obj->value.vector->bytes)) {
      mark_memory(obj->value.vector->bytes);
    } else if(obj->value.vector->elements != NULL &&
              !memory_is_marked(obj->value.vector->elements)) {
      mark_memory(obj->value.vector->elements);
      for(k = 0; k < obj->value.vector->length; k++) {
        mark_push(&obj->value.vector->elements[k]);
      }
    }
    mark_memory(obj->value.vector);
    break;
  case VM_TYPE_EXTERNAL:
    /* The ext_object box is heap-allocated; mark it so the sweep
       phase does not free it while the parent obj is still live.
       The type's optional mark hook is responsible for marking
       opaque_data and anything it transitively references; without
       it, vm_alloc-owned storage hanging off the box would be
       reclaimed by the sweep. */
    if(obj->value.ext_object != NULL) {
      mark_memory(obj->value.ext_object);
      if(obj->value.ext_object->type != NULL &&
         obj->value.ext_object->type->mark != NULL) {
        obj->value.ext_object->type->mark(obj);
      }
    }
    break;
  case VM_TYPE_BOX:
    if(obj->value.box != NULL && !memory_is_marked(obj->value.box)) {
      mark_memory(obj->value.box);
      mark_push(&obj->value.box->value);
    }
    break;
  case VM_TYPE_CLOSURE:
    if(obj->value.closure != NULL && !memory_is_marked(obj->value.closure)) {
      mark_memory(obj->value.closure);
      if(obj->value.closure->captures != NULL &&
         !memory_is_marked(obj->value.closure->captures)) {
        mark_memory(obj->value.closure->captures);
        for(k = 0; k < obj->value.closure->capture_count; k++) {
          mark_push(&obj->value.closure->captures[k]);
        }
      }
    }
    break;
  default:
    break;
  }
}

static void
mark_drain(void)
{
  while(mark_top > 0) {
    mark_expand(mark_stack[--mark_top]);
  }
}

/*
 * Mark one root and everything reachable from it. Draining after each
 * root, rather than pushing all roots first, keeps the work list down to
 * a single root's subgraph.
 */
static void
mark_root(vm_obj_t *obj)
{
  mark_push(obj);
  mark_drain();
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
      mark_root(&expr->argv[j]);
    }

    for(j = 0; j < expr->bindc; j++) {
      VM_DEBUG(VM_DEBUG_HIGH, "GC: Mark bind %d,%d", i, j);
      mark_root(&expr->bindv[j].obj);
    }
  }

  for(i = 0; i < VM_TABLE_SIZE(thread->program->symbols); i++) {
    mark_root(&thread->program->symbol_bindings[i]);
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

  mark_root(&thread->result);
  /* error.error_obj holds the most recent thrown/raised value, populated
     by vm_set_error_string / vm_set_error_object; specific_obj is the
     SRFI-18-style thread-local cell read by (thread-specific); name is
     the SRFI-18 thread name returned by thread-name. All three can
     point at heap-allocated strings or vectors that no other root
     references. */
  mark_root(&thread->error.error_obj);
  mark_root(&thread->specific_obj);
  mark_root(&thread->name);
}

/*
 * Allocate on the heap tier: one block holding the GC header followed by
 * the payload, linked into the list of live heap allocations. Returns
 * the payload address, which is what the rest of the VM sees.
 */
static void *
heap_alloc(unsigned size)
{
  vm_heap_header_t *header;

  /* Nothing else bounds the heap tier now that allocations are not
     entered into a fixed-size table, so hold to VM_HEAP_SIZE here. A
     runaway program then fails with VM_ERROR_HEAP rather than growing
     until the host allocator gives up. */
  if(heap_live_bytes + size > VM_HEAP_SIZE) {
    return NULL;
  }

  header = VM_MALLOC(VM_HEAP_HEADER_SIZE + size);
  if(header == NULL) {
    return NULL;
  }

  header->marked = 0;
  header->size = size;
  header->prev = NULL;
  header->next = heap_list_head;
  if(heap_list_head != NULL) {
    heap_list_head->prev = header;
  }
  heap_list_head = header;

  heap_live_items++;
  heap_live_bytes += size;
  if(heap_live_items > mem_stats.peak_heap_allocations) {
    mem_stats.peak_heap_allocations = heap_live_items;
  }

  return (char *)header + VM_HEAP_HEADER_SIZE;
}

void *
vm_alloc(unsigned size)
{
  vm_thread_t *thread;
  void *ptr;

  thread = vm_current_thread();

  if(size <= VM_POOL_ELEMENT_SIZE) {
    size = VM_POOL_ELEMENT_SIZE;
    ptr = vm_mempool_alloc(&object_pool);
    if(ptr == NULL) {
      /* Try to allocate memory in the regular heap if
         the memory pool is full. */
      vm_gc();
      ptr = heap_alloc(size);
    } else {
      /* Pool slots carry their mark in the pool's own bitmap and need
         no header. */
      mem_stats.mempool_forwards++;
    }
  } else {
    ptr = heap_alloc(size);
  }

  if(ptr == NULL) {
    /* The allocation failed; try to run the garbage collector and then
       make another attempt at allocating the object on the heap. */
    vm_gc();
    ptr = heap_alloc(size);
    if(ptr == NULL) {
      if(thread != NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
      }
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

  if(ptr == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "GC: Attempt to deallocate unknown memory! (%p)",
             ptr);
    return;
  }

  /* No membership test is needed: the caller hands back a pointer that
     vm_alloc returned, and the header in front of it carries what is
     needed to unlink and release it. */
  free_vm_memory(ptr);
  mem_stats.manual_deallocations++;
}

void
vm_free_all(void)
{
  unsigned deallocated;
  vm_ext_object_t *box;
  vm_port_t *port;
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

  /* Close every still-open heap-allocated port for the same reason. */
  while((port = port_list_head) != NULL) {
    port_list_head = port->next;
    if(VM_IS_SET(port->flags, VM_PORT_FLAG_OPEN) &&
       port->io != NULL && port->io->close != NULL) {
      port->io->close(port);
      VM_CLEAR_FLAG(port->flags, VM_PORT_FLAG_OPEN);
    }
  }

  deallocated = 0;
  while(heap_list_head != NULL) {
    vm_heap_header_t *header = heap_list_head;
    heap_list_head = header->next;
    VM_FREE(header);
    deallocated++;
  }
  heap_live_items = 0;
  heap_live_bytes = 0;

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
 * memory here. That happens in the heap sweep that runs immediately
 * after, since the box is a vm_alloc allocation and carries the mark
 * bit in its own header.
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

/*
 * Walk the heap-allocated-ports list and close any port that the mark
 * phase did not reach. The io->close callback releases the underlying
 * fd / opaque_desc; without it the heap sweep would free the port
 * struct without ever closing the resource it owns.
 *
 * As with finalize_unmarked_ext_objects, we unlink the port here but
 * leave the memory free to the heap sweep that runs next.
 */
static void
finalize_unmarked_ports(void)
{
  vm_port_t **link;
  vm_port_t *port;

  link = &port_list_head;
  while((port = *link) != NULL) {
    if(memory_is_marked(port)) {
      link = &port->next;
      continue;
    }
    if(VM_IS_SET(port->flags, VM_PORT_FLAG_OPEN) &&
       port->io != NULL && port->io->close != NULL) {
      port->io->close(port);
      VM_CLEAR_FLAG(port->flags, VM_PORT_FLAG_OPEN);
    }
    *link = port->next;
  }
}

void
vm_port_register(vm_port_t *port)
{
  if(port == NULL) {
    return;
  }
  port->has_peek = 0;
  port->heap_allocated = 1;
  port->next = port_list_head;
  port_list_head = port;
}

static void
do_gc(int force)
{
  unsigned i;
  unsigned deallocated;
  vm_thread_t *thread;
  vm_heap_header_t *header;
  void *free_ptr;
#if VM_PAUSE_PROFILING
  uint64_t pause_start;
  uint64_t pause_ns;
#endif

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
#if VM_PAUSE_PROFILING
  pause_start = pause_now_ns();
#endif

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

  /* Close any port whose only references are gone. The heap sweep
     would otherwise free the port struct without releasing the
     underlying fd / opaque_desc. */
  finalize_unmarked_ports();

  /* Sweep phase: walk the list of live heap allocations, releasing the
     ones the mark phase did not reach and clearing the mark on the ones
     it did. The cost is proportional to the number of allocations rather
     than to the size of the heap. */
  deallocated = 0;
  header = heap_list_head;
  while(header != NULL) {
    vm_heap_header_t *next = header->next;

    if(header->marked) {
      header->marked = 0;
    } else {
      free_ptr = (char *)header + VM_HEAP_HEADER_SIZE;
      VM_DEBUG(VM_DEBUG_HIGH, "GC: Free allocation at address %p", free_ptr);
      heap_unlink(header);
      heap_live_items--;
      heap_live_bytes -= header->size;
      VM_FREE(header);
      deallocated++;
    }
    header = next;
  }

  deallocated += force ? vm_mempool_gc_force(&object_pool)
                       : vm_mempool_gc(&object_pool);

  mem_stats.gc_deallocations += deallocated;

  VM_DEBUG(VM_DEBUG_HIGH, "GC: Deallocated %d of %u objects",
           deallocated,
           (unsigned)(heap_live_items + object_pool.items + deallocated));

  /* Reset memory allocation counter. */
  allocated_since_gc = 0;

#if VM_PAUSE_PROFILING
  pause_ns = pause_now_ns() - pause_start;
  mem_stats.gc_pause_ns_total += pause_ns;
  if(pause_ns > mem_stats.gc_pause_ns_max) {
    mem_stats.gc_pause_ns_max = pause_ns;
  }
  mem_stats.gc_pause_buckets[pause_bucket(pause_ns)]++;
#endif
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

void
vm_gc_mark_pointer(void *ptr)
{
  if(ptr == NULL || memory_is_marked(ptr)) {
    return;
  }
  mark_memory(ptr);
}

void
vm_gc_mark_object(vm_obj_t *obj)
{
  if(obj != NULL) {
    mark_root(obj);
  }
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

  printf("MEM mark_stack peak %lu cap %u overflows %llu\n",
         (unsigned long)mem_stats.mark_stack_peak,
         (unsigned)VM_MARK_STACK_SIZE,
         (unsigned long long)mem_stats.mark_overflows);

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

#if VM_PAUSE_PROFILING
  {
    unsigned k;
    uint64_t mean_ns = mem_stats.gc_invocations > 0
                       ? mem_stats.gc_pause_ns_total / mem_stats.gc_invocations
                       : 0;
    printf("MEM gc_pause_ns total=%llu mean=%llu max=%llu\n",
           (unsigned long long)mem_stats.gc_pause_ns_total,
           (unsigned long long)mean_ns,
           (unsigned long long)mem_stats.gc_pause_ns_max);
    /* Bucket k holds counts of pauses in [2^(k-1), 2^k) microseconds
       for k >= 1; bucket 0 is sub-microsecond. */
    printf("MEM gc_pause_hist");
    for(k = 0; k < VM_PAUSE_BUCKETS; k++) {
      printf(" b%u=%llu", k,
             (unsigned long long)mem_stats.gc_pause_buckets[k]);
    }
    printf("\n");
  }
#endif
}

int
vm_memory_init(void)
{
  VM_DEBUG(VM_DEBUG_MEDIUM,
           "Heap size %u, object pool size %u, pool element size %u",
           VM_HEAP_SIZE, VM_OBJECT_POOL_SIZE, VM_POOL_ELEMENT_SIZE);
  VM_DEBUG(VM_DEBUG_MEDIUM,
           "Heap header %u bytes per allocation, max pool allocations %u",
           (unsigned)VM_HEAP_HEADER_SIZE, VM_MAX_POOL_ALLOCATIONS);

  return vm_mempool_create(&object_pool, VM_POOL_ELEMENT_SIZE,
                           VM_MAX_POOL_ALLOCATIONS);
}
