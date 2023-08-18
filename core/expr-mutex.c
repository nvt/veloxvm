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
#include "vm-list.h"
#include "vm-log.h"

/*
 * There are four mutex states according to SRFI-18:
 *
 * Locked/owned: The mutex is locked, and has an associated owner thread.
 *
 * Locked/not-owned: The mutex is locked, but lacks an owner thread.
 *
 * Unlocked/unabandoned: The mutex has been unlocked by using
 * the "mutex-unlock!" procedure.
 *
 * Unlocked/abandoned: The thread that owned the mutex has been terminated.
 *
 * In this implementation, these states are represented by combining
 * the following three state flags.
 */

#define MUTEX_LOCKED      0x1
#define MUTEX_ABANDONED   0x2
#define MUTEX_HAS_OWNER   0x4

typedef struct wait_thread {
  struct wait_thread *next;
  vm_id_t thread_id;
} wait_thread_t;

typedef struct vm_mutex {
  const char *name;
  wait_thread_t *wait_list;
  vm_obj_t *obj;
  vm_id_t owner_id;
  uint8_t state;
} vm_mutex_t;

static void mutex_create(vm_obj_t *, const char *);
static void mutex_copy(vm_obj_t *, vm_obj_t *);
static void mutex_deallocate(vm_obj_t *);
static void mutex_write(vm_port_t *, vm_obj_t *);

static vm_ext_type_t ext_type_mutex = {
  .copy = mutex_copy,
  .deallocate = mutex_deallocate,
  .write = mutex_write
};

#define VM_PUSH_MUTEX(name) \
          mutex_create(&thread->result, (name))

#define EXTRACT_MUTEX(thread, obj, target_var)               \
  do {                                                       \
    if((obj).type != VM_TYPE_EXTERNAL ||                     \
       (obj).value.ext_object.type != &ext_type_mutex) {     \
        vm_signal_error((thread), VM_ERROR_ARGUMENT_TYPES);  \
        return;                                              \
    }                                                        \
    (target_var) = obj.value.ext_object.opaque_data;         \
  } while(0)

static void
mutex_create(vm_obj_t *dst, const char *name)
{
  vm_mutex_t *mutex;

  mutex = VM_MALLOC(sizeof(vm_mutex_t));
  if(mutex != NULL) {
    dst->type = VM_TYPE_EXTERNAL;
    dst->value.ext_object.type = &ext_type_mutex;
    dst->value.ext_object.opaque_data = mutex;
    mutex->name = name;
    mutex->state = 0;
    mutex->owner_id = VM_ID_INVALID;
    mutex->obj = NULL;
    mutex->wait_list = NULL;
  } else {
    memset(dst, 0, sizeof(vm_obj_t));
    dst->type = VM_TYPE_NONE;
  }
}

static void
mutex_copy(vm_obj_t *dst, vm_obj_t *src)
{
  memcpy(dst, src, sizeof(vm_obj_t));
  /* TO DO: Copy the wait list. */
}

static void
mutex_deallocate(vm_obj_t *obj)
{
  vm_mutex_t *mutex;
  wait_thread_t *wt;

  mutex = obj->value.ext_object.opaque_data;

  /* Deallocate the wait list. */
  while(mutex->wait_list != NULL) {
    wt = mutex->wait_list->next;
    VM_FREE(mutex->wait_list);
    mutex->wait_list = wt;
  }

  VM_FREE(mutex);
}

static void
mutex_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_mutex_t *mutex;

  mutex = obj->value.ext_object.opaque_data;

  vm_write(port, "(#mutex name=\"%s\" state=%u owner=%lu)", mutex->name,
           mutex->state, (unsigned long)mutex->owner_id);
}

VM_FUNCTION(mutexp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_EXTERNAL &&
		  argv[0].value.ext_object.type == &ext_type_mutex);
}

VM_FUNCTION(make_mutex)
{
  VM_PUSH_MUTEX(argv[0].value.string->str);
}

VM_FUNCTION(mutex_name)
{
  vm_mutex_t *mutex;

  EXTRACT_MUTEX(thread, argv[0], mutex);
  vm_string_create(&thread->result, -1, mutex->name);
}

VM_FUNCTION(mutex_specific)
{
  vm_mutex_t *mutex;

  EXTRACT_MUTEX(thread, argv[0], mutex);
  if(mutex->obj != NULL) {
    VM_PUSH(mutex->obj);
  }
}

VM_FUNCTION(mutex_specific_set)
{
  vm_mutex_t *mutex;

  EXTRACT_MUTEX(thread, argv[0], mutex);
  mutex->obj = VM_MALLOC(sizeof(vm_obj_t));
  if(mutex->obj == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    memcpy(mutex->obj, &argv[1], sizeof(vm_obj_t));
  }
}

VM_FUNCTION(mutex_state)
{
  vm_mutex_t *mutex;
  vm_thread_t *owner;

  EXTRACT_MUTEX(thread, argv[0], mutex);

  if(IS_SET(mutex->state, MUTEX_LOCKED)) {
    if(IS_SET(mutex->state, MUTEX_HAS_OWNER)) {
      /* The mutex is locked; return the owner thread. */
      owner = vm_thread_get(mutex->owner_id);
      if(owner == NULL) {
	vm_signal_error(thread, VM_ERROR_INTERNAL);
	return;
      }
      thread_obj_create(&thread->result, owner);
    } else {
      /* The mutex is locked, but has no owner; return the NOT-OWNED symbol. */
    }
    return;
  }

  /*
   * The mutex is not locked if this point is reached. Return either
   * the ABANDONED or NOT-ABANDONED symbol depending on whether the
   * owner thread has been terminated.
   */
  if(IS_SET(mutex->state, MUTEX_ABANDONED)) {
    /* TO DO: Fix symbol injection. */
    (void)0;
  } else {
    (void)0;
  }
}

VM_FUNCTION(mutex_lock)
{
  vm_mutex_t *mutex;
  wait_thread_t *wt;
  wait_thread_t *wt_iter;

  EXTRACT_MUTEX(thread, argv[0], mutex);

  if(IS_CLEAR(mutex->state, MUTEX_LOCKED)) {
    SET(mutex->state, MUTEX_LOCKED);
    mutex->owner_id = thread->id;

    if(IS_SET(mutex->state, MUTEX_ABANDONED)) {
      /* The mutex has been abandoned, so an "abandoned mutex exception"
         must be raised in the locking thread after locking the mutex. */

      /* TO DO: Change the exception object from the mutex to a symbol. */
      vm_raise_exception(thread, &argv[0]);
    }

    VM_DEBUG(VM_DEBUG_MEDIUM, "Locked mutex \"%s\"", mutex->name);

    VM_PUSH_BOOLEAN(VM_TRUE);
    return;
  }

  /* The mutex is locked, so put the thread at the end of the wait list. */

  wt = VM_MALLOC(sizeof(wait_thread_t));
  if(wt == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  wt->thread_id = thread->id;
  wt->next = NULL;

  /* Put the thread at the tail of the wait list. */
  if(mutex->wait_list == NULL) {
    mutex->wait_list = wt;
  } else {
    for(wt_iter = mutex->wait_list;
	wt_iter->next != NULL;
	wt_iter = wt_iter->next);
    wt_iter->next = wt;
  }

  VM_DEBUG(VM_DEBUG_MEDIUM, "Waiting for mutex \"%s\"", mutex->name);

  thread->status = VM_THREAD_WAITING;
}

VM_FUNCTION(mutex_unlock)
{
  vm_mutex_t *mutex;
  vm_thread_t *lock_thread;
  wait_thread_t *wt;

  /* Any thread may unlock a mutex, even if it is not the owner. An already
     unlocked mutex may also be unlocked again. */

  EXTRACT_MUTEX(thread, argv[0], mutex);

  VM_DEBUG(VM_DEBUG_MEDIUM, "Unlocked mutex \"%s\"", mutex->name);

  /* If there are threads on the waiting list, we immediately let the
     next one in the queue lock the mutex. */
  while(mutex->wait_list != NULL) {
    lock_thread = vm_thread_get(mutex->wait_list->thread_id);
    wt = mutex->wait_list->next;
    VM_FREE(mutex->wait_list);
    mutex->wait_list = wt;

    if(lock_thread != NULL) {
      /* We found the next thread that can lock the mutex. */
      mutex->owner_id = lock_thread->id;
      lock_thread->status = VM_THREAD_RUNNABLE;
      VM_DEBUG(VM_DEBUG_MEDIUM,
	       "Immediately locked mutex \"%s\" for thread %lu",
	       mutex->name, (unsigned long)mutex->owner_id);
      return;
    }
  }

  /* There was no thread that could lock the mutex, so we place it in
     the unlocked state. */
  CLEAR(mutex->state, MUTEX_LOCKED);
}
