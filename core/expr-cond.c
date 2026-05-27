/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
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
 * SRFI-18-style condition variables. Pair with mutexes for the
 * canonical wait/signal idiom: a thread holds the mutex, decides it
 * cannot proceed, and atomically unlocks the mutex + parks itself on
 * the cv via (mutex-unlock! m cv [timeout]). The atomicity matters
 * to avoid the classic missed-wakeup race between unlock and wait.
 */

#include "vm-functions.h"
#include "vm-cond.h"
#include "vm-log.h"
#include "vm-native.h"

typedef struct cv_waiter {
  struct cv_waiter *next;
  vm_id_t thread_id;
} cv_waiter_t;

typedef struct vm_cond {
  /* Owned heap copy of the name. */
  char *name;
  cv_waiter_t *wait_list;
  /* SRFI-18-style application-specific cell. type == VM_TYPE_NONE
     means "unset" (the slot returned by condition-variable-specific
     before any -set! call); the GC mark hook walks this field so
     heap objects stashed here stay live. */
  vm_obj_t specific;
} vm_cond_t;

static void cond_copy(vm_obj_t *, vm_obj_t *);
static void cond_deallocate(vm_obj_t *);
static void cond_write(vm_port_t *, vm_obj_t *);
static void cond_mark(vm_obj_t *);

static vm_ext_type_t ext_type_cond = {
  .copy = cond_copy,
  .deallocate = cond_deallocate,
  .write = cond_write,
  .mark = cond_mark
};

#define EXTRACT_COND(thread, obj, target_var)                \
  do {                                                       \
    if((obj).type != VM_TYPE_EXTERNAL ||                     \
       (obj).value.ext_object->type != &ext_type_cond) {     \
        vm_signal_error((thread), VM_ERROR_ARGUMENT_TYPES);  \
        return;                                              \
    }                                                        \
    (target_var) = (obj).value.ext_object->opaque_data;      \
  } while(0)

static char *
cond_strdup(const char *src)
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
cond_copy(vm_obj_t *dst, vm_obj_t *src)
{
  /* CVs are reference types: copying the vm_obj_t shares the box
     and the wait list by design. */
  memcpy(dst, src, sizeof(vm_obj_t));
}

static void
cond_deallocate(vm_obj_t *obj)
{
  vm_cond_t *cv;
  cv_waiter_t *waiter;

  cv = obj->value.ext_object->opaque_data;
  while(cv->wait_list != NULL) {
    waiter = cv->wait_list->next;
    VM_FREE(cv->wait_list);
    cv->wait_list = waiter;
  }
  VM_FREE(cv->name);
  VM_FREE(cv);
}

static void
cond_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_cond_t *cv;
  cv = obj->value.ext_object->opaque_data;
  vm_write(port, "#<condition-variable %s>",
           cv->name != NULL ? cv->name : "");
}

static void
cond_mark(vm_obj_t *obj)
{
  vm_cond_t *cv;

  cv = obj->value.ext_object->opaque_data;
  vm_gc_mark_pointer(cv);
  if(cv->name != NULL) {
    vm_gc_mark_pointer(cv->name);
  }
  /* Walk the specific cell so heap objects stashed via
     condition-variable-specific-set! stay alive as long as the cv
     does. Same treatment as mutex's inline specific. */
  vm_gc_mark_object(&cv->specific);
}

/* Remove a thread from this CV's wait list (idempotent). Used by
   signal/broadcast as they pop waiters, and by cond_cancel_wait when
   a timeout fires before any signal reaches the parked thread. */
static void
cond_remove_waiter(vm_cond_t *cv, vm_id_t thread_id)
{
  cv_waiter_t **link;
  cv_waiter_t *waiter;

  for(link = &cv->wait_list; (waiter = *link) != NULL; link = &waiter->next) {
    if(waiter->thread_id == thread_id) {
      *link = waiter->next;
      VM_FREE(waiter);
      return;
    }
  }
}

/* wait_cancel callback. Invoked by the timer-fire path before status
   is flipped to RUNNABLE. The parent argv slot was overwritten with
   the operator's default VM_TYPE_NONE result when the (mutex-unlock!
   m cv timeout) frame was popped; rewrite it to #f so the form
   returns the right boolean on the timeout path. */
static void
cond_cancel_wait(vm_thread_t *thread)
{
  vm_cond_t *cv;
  vm_expr_t *frame;

  cv = thread->wait_object;
  if(cv != NULL) {
    cond_remove_waiter(cv, thread->id);
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

/* Wake a single waiter currently parked on this CV. Returns 1 if a
   waiter was found and woken, 0 if the list was empty. */
static int
cond_wake_one(vm_cond_t *cv)
{
  cv_waiter_t *waiter;
  vm_thread_t *target;
  vm_expr_t *frame;

  while((waiter = cv->wait_list) != NULL) {
    cv->wait_list = waiter->next;
    target = vm_thread_get(waiter->thread_id);
    VM_FREE(waiter);
    if(target == NULL) {
      /* The waiter was destroyed in between -- keep looking. */
      continue;
    }
    /* Disarm timer-side cancellation; if the wakeup timer fires
       later it will see status != WAITING and skip. */
    target->wait_cancel = NULL;
    target->wait_object = NULL;
    target->wait_outcome = VM_WAIT_OUTCOME_SIGNALED;
    target->result.type = VM_TYPE_BOOLEAN;
    target->result.value.boolean = VM_TRUE;
    frame = target->expr;
    if(frame != NULL && frame->eval_arg < frame->argc) {
      memcpy(&frame->argv[frame->eval_arg], &target->result,
             sizeof(vm_obj_t));
    }
    if(target->status == VM_THREAD_WAITING) {
      target->status = VM_THREAD_RUNNABLE;
    }
    return 1;
  }
  return 0;
}

int
vm_cond_check_type(const vm_obj_t *obj)
{
  return obj->type == VM_TYPE_EXTERNAL &&
         obj->value.ext_object != NULL &&
         obj->value.ext_object->type == &ext_type_cond;
}

int
vm_cond_park_thread(vm_obj_t *cv_obj, vm_thread_t *thread,
                    vm_integer_t timeout_ms)
{
  vm_cond_t *cv;
  cv_waiter_t *waiter;

  if(!vm_cond_check_type(cv_obj)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return 0;
  }
  cv = cv_obj->value.ext_object->opaque_data;

  waiter = VM_MALLOC(sizeof(cv_waiter_t));
  if(waiter == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return 0;
  }
  waiter->thread_id = thread->id;
  /* FIFO: append at the tail so signal wakes the oldest waiter
     first. The list is short in practice (one entry per parked
     thread), so the O(n) walk per park is negligible. */
  if(cv->wait_list == NULL) {
    waiter->next = NULL;
    cv->wait_list = waiter;
  } else {
    cv_waiter_t *iter;
    for(iter = cv->wait_list; iter->next != NULL; iter = iter->next);
    iter->next = waiter;
    waiter->next = NULL;
  }

  thread->wait_object = cv;
  thread->wait_cancel = cond_cancel_wait;
  thread->wait_outcome = VM_WAIT_OUTCOME_NONE;
  /* If a timeout is requested, vm_native_sleep arms a timer that
     fires after timeout_ms; the timer-fire path will call
     wait_cancel (cond_cancel_wait) if no signal has reached us
     first. A negative timeout means "wait indefinitely" -- no
     timer is scheduled, only signal/broadcast can wake us. */
  if(timeout_ms >= 0) {
    vm_native_sleep(thread, timeout_ms);
  } else {
    thread->status = VM_THREAD_WAITING;
  }
  return 1;
}

VM_FUNCTION(condition_variablep)
{
  VM_PUSH_BOOLEAN(vm_cond_check_type(&argv[0]));
}

VM_FUNCTION(make_condition_variable)
{
  vm_cond_t *cv;
  const char *name;

  /* SRFI 18 allows the name argument to be omitted; we accept either
     no args (anonymous) or one string. */
  if(argc == 0) {
    name = "";
  } else {
    if(argv[0].type != VM_TYPE_STRING) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    name = argv[0].value.string->str;
  }

  cv = VM_MALLOC(sizeof(vm_cond_t));
  if(cv == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  cv->name = cond_strdup(name);
  if(cv->name == NULL) {
    VM_FREE(cv);
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  cv->wait_list = NULL;
  cv->specific.type = VM_TYPE_NONE;
  if(vm_ext_object_create(&thread->result, &ext_type_cond, cv) == NULL) {
    VM_FREE(cv->name);
    VM_FREE(cv);
  }
}

VM_FUNCTION(condition_variable_name)
{
  vm_cond_t *cv;

  EXTRACT_COND(thread, argv[0], cv);
  vm_string_create(&thread->result, -1, cv->name);
}

VM_FUNCTION(condition_variable_signal)
{
  vm_cond_t *cv;

  EXTRACT_COND(thread, argv[0], cv);
  cond_wake_one(cv);
}

VM_FUNCTION(condition_variable_broadcast)
{
  vm_cond_t *cv;

  EXTRACT_COND(thread, argv[0], cv);
  while(cond_wake_one(cv)) {
    /* loop until the wait list is empty */
  }
}

VM_FUNCTION(condition_variable_specific)
{
  vm_cond_t *cv;

  EXTRACT_COND(thread, argv[0], cv);
  VM_PUSH(&cv->specific);
}

VM_FUNCTION(condition_variable_specific_set)
{
  vm_cond_t *cv;

  EXTRACT_COND(thread, argv[0], cv);
  memcpy(&cv->specific, &argv[1], sizeof(vm_obj_t));
}
