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
  vm_thread_t *new_thread;

  new_thread = vm_thread_fork(thread);
  if(new_thread == NULL) {
    vm_signal_error(thread, VM_ERROR_THREAD);
  } else {
    new_thread->result.type = VM_TYPE_INTEGER;
    new_thread->result.value.integer = 0;
    thread_obj_create(&thread->result, new_thread);
  }
}

VM_FUNCTION(thread_id)
{
  VM_PUSH_INTEGER(thread->id);
}

VM_FUNCTION(thread_join)
{
}

VM_FUNCTION(thread_sleep)
{
  VM_DEBUG(VM_DEBUG_MEDIUM, "Sleeping %lu ms",
           (unsigned long)argv[0].value.integer);
  if(argv[0].value.integer <= 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[0]);
  } else {
    vm_native_sleep(thread, argv[0].value.integer);
  }
}

VM_FUNCTION(thread_specific)
{
  vm_thread_id_t id;
  vm_thread_t *target_thread;

  id = argv[0].value.integer;
  target_thread = vm_thread_get(id);
  if(target_thread == NULL) {
    vm_signal_error(thread, VM_ERROR_THREAD);
  } else {
    VM_PUSH(&target_thread->specific_obj);
  }
}

VM_FUNCTION(thread_specific_set)
{
  vm_thread_id_t id;
  vm_thread_t *target_thread;

  id = argv[0].value.integer;
  target_thread = vm_thread_get(id);
  if(target_thread == NULL) {
    vm_signal_error(thread, VM_ERROR_THREAD);
  } else {
    memcpy(&target_thread->specific_obj, &argv[1], sizeof(vm_obj_t));
  }
}

VM_FUNCTION(thread_terminate)
{
  if(argv[0].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  } else {
    VM_PUSH_BOOLEAN(vm_thread_kill(argv[0].value.integer));
  }
}

VM_FUNCTION(thread_yield)
{
}

VM_FUNCTION(thread_stats)
{
  vm_thread_id_t id;
  vm_thread_t *target_thread;
  vm_vector_t *vector;
  vm_obj_t element;
  int r;

  if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_STATS)) {
    return;
  }

  if(argc == 1) {
    id = argv[0].value.integer;
    target_thread = vm_thread_get(id);
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
