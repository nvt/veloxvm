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
#include "vm-cond.h"
#include "vm-log.h"
#include "vm-native.h"

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
  /* Heap copy of the mutex name, owned by this struct. The caller's
     argument may be a string loaded from the program's string table
     (which the GC can sweep), so we cannot borrow its buffer. */
  char *name;
  wait_thread_t *wait_list;
  /* Inline specific value. type == VM_TYPE_NONE means "unset"; the
     GC mark hook walks this slot so heap objects stashed via
     mutex-specific-set! stay live as long as the mutex does. */
  vm_obj_t specific;
  vm_id_t owner_id;
  uint8_t state;
} vm_mutex_t;

static void mutex_create(vm_obj_t *, const char *);
static void mutex_copy(vm_obj_t *, vm_obj_t *);
static void mutex_deallocate(vm_obj_t *);
static void mutex_write(vm_port_t *, vm_obj_t *);
static void mutex_mark(vm_obj_t *);

static vm_ext_type_t ext_type_mutex = {
  .copy = mutex_copy,
  .deallocate = mutex_deallocate,
  .write = mutex_write,
  .mark = mutex_mark
};

#define VM_PUSH_MUTEX(name) \
          mutex_create(&thread->result, (name))

#define EXTRACT_MUTEX(thread, obj, target_var)               \
  do {                                                       \
    if((obj).type != VM_TYPE_EXTERNAL ||                     \
       (obj).value.ext_object->type != &ext_type_mutex) {    \
        vm_signal_error((thread), VM_ERROR_ARGUMENT_TYPES);  \
        return;                                              \
    }                                                        \
    (target_var) = obj.value.ext_object->opaque_data;        \
  } while(0)

static char *
mutex_strdup(const char *src)
{
  size_t len;
  char *dst;

  if(src == NULL) {
    src = "";
  }
  len = strlen(src);
  dst = VM_MALLOC(len + 1);
  if(dst == NULL) {
    return NULL;
  }
  memcpy(dst, src, len + 1);
  return dst;
}

static void
mutex_create(vm_obj_t *dst, const char *name)
{
  vm_mutex_t *mutex;

  mutex = VM_MALLOC(sizeof(vm_mutex_t));
  if(mutex == NULL) {
    memset(dst, 0, sizeof(vm_obj_t));
    dst->type = VM_TYPE_NONE;
    return;
  }
  mutex->name = mutex_strdup(name);
  if(mutex->name == NULL) {
    VM_FREE(mutex);
    memset(dst, 0, sizeof(vm_obj_t));
    dst->type = VM_TYPE_NONE;
    return;
  }
  mutex->state = 0;
  mutex->owner_id = VM_ID_INVALID;
  mutex->specific.type = VM_TYPE_NONE;
  mutex->wait_list = NULL;

  if(vm_ext_object_create(dst, &ext_type_mutex, mutex) == NULL) {
    VM_FREE(mutex->name);
    VM_FREE(mutex);
  }
}

static void
mutex_copy(vm_obj_t *dst, vm_obj_t *src)
{
  /* Mutexes are reference types: copying the vm_obj_t shares the box
     and its underlying state by design. The wait list is part of
     that shared state and must not be duplicated. */
  memcpy(dst, src, sizeof(vm_obj_t));
}

static void
mutex_deallocate(vm_obj_t *obj)
{
  vm_mutex_t *mutex;
  wait_thread_t *wt;

  mutex = obj->value.ext_object->opaque_data;

  /* Deallocate the wait list. */
  while(mutex->wait_list != NULL) {
    wt = mutex->wait_list->next;
    VM_FREE(mutex->wait_list);
    mutex->wait_list = wt;
  }

  VM_FREE(mutex->name);
  VM_FREE(mutex);
}

static void
mutex_mark(vm_obj_t *obj)
{
  vm_mutex_t *mutex;

  mutex = obj->value.ext_object->opaque_data;
  /* Keep the mutex struct itself live; without this the heap sweep
     frees it while the ext_object box still references it. */
  vm_gc_mark_pointer(mutex);
  if(mutex->name != NULL) {
    vm_gc_mark_pointer(mutex->name);
  }
  /* Mark anything reachable via mutex-specific (heap strings,
     vectors, pairs, ...) so it survives as long as the mutex does. */
  vm_gc_mark_object(&mutex->specific);
}

static void
mutex_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_mutex_t *mutex;

  mutex = obj->value.ext_object->opaque_data;

  vm_write(port, "(#mutex name=\"%s\" state=%u owner=%lu)", mutex->name,
           mutex->state, (unsigned long)mutex->owner_id);
}

VM_FUNCTION(mutexp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_EXTERNAL &&
		  argv[0].value.ext_object->type == &ext_type_mutex);
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
  VM_PUSH(&mutex->specific);
}

VM_FUNCTION(mutex_specific_set)
{
  vm_mutex_t *mutex;

  EXTRACT_MUTEX(thread, argv[0], mutex);
  memcpy(&mutex->specific, &argv[1], sizeof(vm_obj_t));
}

VM_FUNCTION(mutex_state)
{
  vm_mutex_t *mutex;
  vm_thread_t *owner;

  EXTRACT_MUTEX(thread, argv[0], mutex);

  if(VM_IS_SET(mutex->state, MUTEX_LOCKED)) {
    if(VM_IS_SET(mutex->state, MUTEX_HAS_OWNER)) {
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
  if(VM_IS_SET(mutex->state, MUTEX_ABANDONED)) {
    /* TO DO: Fix symbol injection. */
    (void)0;
  } else {
    (void)0;
  }
}

static void
mutex_remove_waiter(vm_mutex_t *mutex, vm_id_t thread_id)
{
  wait_thread_t **link;
  wait_thread_t *wt;

  for(link = &mutex->wait_list;
      (wt = *link) != NULL;
      link = &wt->next) {
    if(wt->thread_id == thread_id) {
      *link = wt->next;
      VM_FREE(wt);
      return;
    }
  }
}

/* wait_cancel for a thread parked in mutex-lock! with a timeout that
   fires before the lock can be handed to us. Unhook self from the
   mutex's wait list and write #f into the parent argv slot so the
   form returns #f on the timeout path. */
static void
mutex_lock_cancel_wait(vm_thread_t *thread)
{
  vm_mutex_t *mutex;
  vm_expr_t *frame;

  mutex = thread->wait_object;
  if(mutex != NULL) {
    mutex_remove_waiter(mutex, thread->id);
  }
  thread->wait_object = NULL;
  thread->wait_cancel = NULL;
  thread->wait_outcome = VM_WAIT_OUTCOME_TIMEOUT;
  thread->result.type = VM_TYPE_BOOLEAN;
  thread->result.value.boolean = VM_FALSE;
  frame = thread->expr;
  if(frame != NULL && frame->eval_arg < frame->argc) {
    memcpy(&frame->argv[frame->eval_arg], &thread->result,
           sizeof(vm_obj_t));
  }
}

/* Hand the mutex off to a parked waiter as part of mutex-unlock!.
   Mirrors cond_wake_one in expr-cond.c: clears the waiter's wait_cancel
   so a racing timeout sees status != WAITING and skips, writes #t into
   the waiter's parent argv slot so (mutex-lock! m timeout) returns #t
   on the signal path, and flips status to RUNNABLE. */
static int
mutex_hand_off(vm_mutex_t *mutex)
{
  wait_thread_t *wt;
  vm_thread_t *lock_thread;
  vm_expr_t *frame;

  while((wt = mutex->wait_list) != NULL) {
    lock_thread = vm_thread_get(wt->thread_id);
    mutex->wait_list = wt->next;
    VM_FREE(wt);
    if(lock_thread == NULL) {
      continue;
    }
    mutex->owner_id = lock_thread->id;
    lock_thread->wait_cancel = NULL;
    lock_thread->wait_object = NULL;
    lock_thread->wait_outcome = VM_WAIT_OUTCOME_SIGNALED;
    lock_thread->result.type = VM_TYPE_BOOLEAN;
    lock_thread->result.value.boolean = VM_TRUE;
    frame = lock_thread->expr;
    if(frame != NULL && frame->eval_arg < frame->argc) {
      memcpy(&frame->argv[frame->eval_arg], &lock_thread->result,
             sizeof(vm_obj_t));
    }
    if(lock_thread->status == VM_THREAD_WAITING) {
      lock_thread->status = VM_THREAD_RUNNABLE;
    }
    VM_DEBUG(VM_DEBUG_MEDIUM,
             "Handed mutex \"%s\" to thread %lu",
             mutex->name, (unsigned long)mutex->owner_id);
    return 1;
  }
  return 0;
}

VM_FUNCTION(mutex_lock)
{
  vm_mutex_t *mutex;
  wait_thread_t *wt;
  wait_thread_t *wt_iter;
  vm_integer_t timeout_ms;

  EXTRACT_MUTEX(thread, argv[0], mutex);

  /* SRFI-18-style optional timeout. A negative value (or omitted)
     means "wait indefinitely"; 0 means "try acquire, return #f on
     contention without parking"; positive is a wait deadline in ms.
     A non-integer/non-boolean timeout argument is rejected. */
  timeout_ms = -1;
  if(argc >= 2) {
    if(argv[1].type == VM_TYPE_INTEGER) {
      timeout_ms = argv[1].value.integer;
    } else if(argv[1].type == VM_TYPE_BOOLEAN) {
      /* #f = no timeout (SRFI 18). Any other boolean is treated the
         same; SRFI 18 only defines #f as the "no timeout" sentinel. */
      timeout_ms = -1;
    } else {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
  }

  if(VM_IS_CLEAR(mutex->state, MUTEX_LOCKED)) {
    VM_SET_FLAG(mutex->state, MUTEX_LOCKED);
    mutex->owner_id = thread->id;

    if(VM_IS_SET(mutex->state, MUTEX_ABANDONED)) {
      /* The mutex has been abandoned, so an "abandoned mutex exception"
         must be raised in the locking thread after locking the mutex. */

      /* TO DO: Change the exception object from the mutex to a symbol. */
      vm_raise_exception(thread, &argv[0]);
    }

    VM_DEBUG(VM_DEBUG_MEDIUM, "Locked mutex \"%s\"", mutex->name);

    VM_PUSH_BOOLEAN(VM_TRUE);
    return;
  }

  if(timeout_ms == 0) {
    /* Poll path: contended and the caller does not want to wait. */
    VM_PUSH_BOOLEAN(VM_FALSE);
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

  /* Register the cancellation hook so a racing timeout pulls us
     back out of the wait list and surfaces #f. The hook is cleared
     by mutex_hand_off on the signal path. */
  thread->wait_object = mutex;
  thread->wait_cancel = mutex_lock_cancel_wait;
  thread->wait_outcome = VM_WAIT_OUTCOME_NONE;

  if(timeout_ms > 0) {
    /* vm_native_sleep arms the wake timer and sets status WAITING. */
    vm_native_sleep(thread, timeout_ms);
  } else {
    thread->status = VM_THREAD_WAITING;
  }
}

VM_FUNCTION(mutex_unlock)
{
  vm_mutex_t *mutex;
  vm_integer_t timeout_ms;

  /* Any thread may unlock a mutex, even if it is not the owner. An already
     unlocked mutex may also be unlocked again. */

  EXTRACT_MUTEX(thread, argv[0], mutex);

  VM_DEBUG(VM_DEBUG_MEDIUM, "Unlocked mutex \"%s\"", mutex->name);

  /* Release the mutex before parking on the condition variable. The
     SRFI-18 unlock-cv-wait atomicity boundary is "the calling thread
     is added to the cv before the mutex is unlocked" -- but because
     VeloxVM is single-threaded with cooperative+preemptive
     scheduling, no other thread can observe an intermediate state
     between these two operations. Either order is safe; releasing
     first keeps the no-cv case identical to before. */
  if(!mutex_hand_off(mutex)) {
    /* No waiters wanted the mutex, so place it in the unlocked
       state. */
    VM_CLEAR_FLAG(mutex->state, MUTEX_LOCKED);
  }

  if(argc < 2) {
    /* The classic (mutex-unlock! m) form: release and return. */
    return;
  }

  /* SRFI-18 (mutex-unlock! mutex cv [timeout]) atomically releases
     the mutex and parks the caller on cv. Returns #t on signal, #f
     on timeout (the boolean is propagated into the parent argv slot
     by cond_wake_one / cond_cancel_wait). */
  if(!vm_cond_check_type(&argv[1])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  timeout_ms = -1;
  if(argc >= 3) {
    if(argv[2].type == VM_TYPE_INTEGER) {
      timeout_ms = argv[2].value.integer;
    } else if(argv[2].type == VM_TYPE_BOOLEAN) {
      timeout_ms = -1;
    } else {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
  }

  if(timeout_ms == 0) {
    /* SRFI 18 timeout of 0 means "do not block"; return #f
       immediately without enrolling on the cv. */
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }

  vm_cond_park_thread(&argv[1], thread, timeout_ms);
}
