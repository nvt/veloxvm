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

#include "vm-functions.h"
#include "vm-log.h"
#include "vm-native.h"

VM_FUNCTION(thread_create)
{
  vm_thread_t *new_thread;

  new_thread = vm_thread_spawn(thread, &argv[0]);
  if(new_thread == NULL) {
    vm_signal_error(thread, VM_ERROR_THREAD);
  } else {
    thread_obj_create(&thread->result, new_thread);
  }
}

VM_FUNCTION(thread_fork)
{
  /* thread-fork! is retired. The previous implementation copied the
     parent's frame stack into the child but shared bindv pointers,
     producing double-frees on pop, and never set the child's exprc,
     leaving its frames invisible to the scheduler and GC. The slot
     is kept in the operator table to preserve primitive IDs of
     subsequent primitives; calling it now raises a clear error. */
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
  vm_set_error_string(thread, "thread-fork! has been retired; "
                              "use thread-create! instead");
}

VM_FUNCTION(thread_id)
{
  VM_PUSH_INTEGER(thread->id);
}

VM_FUNCTION(threadp)
{
  VM_PUSH_BOOLEAN(vm_thread_from_object(&argv[0]) != NULL);
}

VM_FUNCTION(current_thread)
{
  thread_obj_create(&thread->result, thread);
}

VM_FUNCTION(thread_join)
{
  vm_thread_t *target;
  vm_thread_joiner_t *waiter;

  target = vm_thread_from_object(&argv[0]);
  if(target == NULL) {
    /* The handle points at a thread that has already finished and
       been destroyed. Its result is no longer available; return #f
       so callers binding the result through (define x ...) get a
       defined value rather than triggering an undefined-symbol
       error on the next reference. */
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }

  if(target == thread) {
    /* SRFI 18 leaves self-join undefined. Raising is more useful
       than deadlocking. */
    vm_signal_error(thread, VM_ERROR_THREAD);
    vm_set_error_string(thread, "thread-join!: a thread may not join on itself");
    return;
  }

  if(target->status == VM_THREAD_FINISHED ||
     target->status == VM_THREAD_ERROR ||
     target->status == VM_THREAD_EXITING) {
    /* The joinee has run to completion since the handle was
       constructed but the scheduler has not yet finalized it.
       Surface its result directly; vm_thread_finalize_joiners would
       do the same once the joinee is processed. */
    memcpy(&thread->result, &target->result, sizeof(vm_obj_t));
    return;
  }

  waiter = VM_MALLOC(sizeof(vm_thread_joiner_t));
  if(waiter == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  waiter->joiner_id = thread->id;
  waiter->next = target->joiners;
  target->joiners = waiter;

  thread->status = VM_THREAD_WAITING;
}

VM_FUNCTION(thread_sleep)
{
  VM_DEBUG(VM_DEBUG_MEDIUM, "Sleeping %lu ms",
           (unsigned long)argv[0].value.integer);
  if(argv[0].value.integer < 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[0]);
  } else if(argv[0].value.integer == 0) {
    /* SRFI 18: a 0-ms sleep is a yield. Defer the rest of this
       per-invocation slice to the next runnable thread; status
       stays RUNNABLE so vm_run brings us back next round. */
    thread->yield_requested = 1;
  } else {
    vm_native_sleep(thread, argv[0].value.integer);
  }
}

VM_FUNCTION(thread_specific)
{
  vm_thread_t *target;

  target = vm_thread_from_object(&argv[0]);
  if(target == NULL) {
    vm_signal_error(thread, VM_ERROR_THREAD);
    return;
  }
  VM_PUSH(&target->specific_obj);
}

VM_FUNCTION(thread_specific_set)
{
  vm_thread_t *target;

  target = vm_thread_from_object(&argv[0]);
  if(target == NULL) {
    vm_signal_error(thread, VM_ERROR_THREAD);
    return;
  }
  memcpy(&target->specific_obj, &argv[1], sizeof(vm_obj_t));
}

VM_FUNCTION(thread_terminate)
{
  vm_thread_t *target;

  target = vm_thread_from_object(&argv[0]);
  if(target == NULL) {
    /* Handle to a thread that has already finished. Returning #f
       lets idempotent shutdown patterns -- terminate-then-poll, or
       terminate-on-cleanup that races with natural completion --
       distinguish "I killed it" from "it was already gone" without
       crashing the caller. */
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }
  VM_PUSH_BOOLEAN(vm_thread_kill(target->id));
}

VM_FUNCTION(thread_yield)
{
  /* The scheduler clears yield_requested on observation and breaks
     out of its per-invocation instruction-quota loop, handing the
     remainder of this slice to the next runnable thread. */
  thread->yield_requested = 1;
}

VM_FUNCTION(thread_stats)
{
  vm_thread_t *target_thread;
  vm_vector_t *vector;
  vm_obj_t element;
  int r;

  if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_STATS)) {
    return;
  }

  if(argc == 1) {
    target_thread = vm_thread_from_object(&argv[0]);
    if(target_thread == NULL) {
      vm_signal_error(thread, VM_ERROR_THREAD);
      return;
    }
  } else {
    target_thread = thread;
  }

  vector = vm_vector_create(&thread->result, 3, VM_VECTOR_FLAG_REGULAR);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  /* Sum the return values of vm_vector_set() below, to ensure that
     all elements were properly set. */
  r = 0;

  /* Fill the statistics vector. */
  element.type = VM_TYPE_INTEGER;

  element.value.integer = target_thread->stats.schedulings;
  r += vm_vector_set(&thread->result, 0, &element);

  element.value.integer = target_thread->stats.function_calls;
  r += vm_vector_set(&thread->result, 1, &element);

  element.value.integer = target_thread->stats.allocated_total;
  r += vm_vector_set(&thread->result, 2, &element);

  if(r != 3) {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
  }
}
