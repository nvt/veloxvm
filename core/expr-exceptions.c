/* Copyright (c) 2012-2017, RISE SICS AB
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

#include "vm-log.h"
#include "vm-functions.h"
#include "vm-exceptions.h"

VM_FUNCTION(guard)
{
  if(argv[0].type != VM_TYPE_SYMBOL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  if(VM_EVAL_COMPLETED(thread, 2)) {
    /* Executed the exception handler -- return its result. */
    /* Clear the handler flag before returning (R6RS/R7RS compliance) */
    thread->expr->flags &= ~VM_EXPR_GUARD_IN_HANDLER;
    VM_PUSH(&argv[1]);
  } else if(VM_EVAL_COMPLETED(thread, 3)) {
    /* Executed the main expression -- return its result. */
    /* Clear the handler flag (defensive - should already be clear) */
    thread->expr->flags &= ~VM_EXPR_GUARD_IN_HANDLER;
    VM_PUSH(&argv[2]);
  } else if(!VM_EVAL_REQUESTED(thread, 1)) {
    /* Evaluate the main expression as long as the guard handler has not
       been activated. */
    VM_EVAL_ARG(thread, 2);
  }
}

VM_FUNCTION(raise)
{
  vm_raise_exception(thread, &argv[0]);
}

/* --- SRFI 18 typed exceptions ----------------------------------- */

typedef struct vm_srfi18_exception {
  vm_srfi18_exception_kind_t kind;
  /* Used by VM_SRFI18_UNCAUGHT_EXCEPTION to wrap the original
     exception object. For other kinds, type == VM_TYPE_NONE. */
  vm_obj_t reason;
} vm_srfi18_exception_t;

static void srfi18_copy(vm_obj_t *, vm_obj_t *);
static void srfi18_deallocate(vm_obj_t *);
static void srfi18_write(vm_port_t *, vm_obj_t *);
static void srfi18_mark(vm_obj_t *);

static vm_ext_type_t ext_type_srfi18_exception = {
  .copy = srfi18_copy,
  .deallocate = srfi18_deallocate,
  .write = srfi18_write,
  .mark = srfi18_mark
};

static const char *
srfi18_name(vm_srfi18_exception_kind_t kind)
{
  switch(kind) {
  case VM_SRFI18_JOIN_TIMEOUT:       return "join-timeout-exception";
  case VM_SRFI18_ABANDONED_MUTEX:    return "abandoned-mutex-exception";
  case VM_SRFI18_TERMINATED_THREAD:  return "terminated-thread-exception";
  case VM_SRFI18_UNCAUGHT_EXCEPTION: return "uncaught-exception";
  }
  return "srfi18-exception";
}

static void
srfi18_copy(vm_obj_t *dst, vm_obj_t *src)
{
  /* Exception conditions are immutable; sharing the box is sound. */
  memcpy(dst, src, sizeof(vm_obj_t));
}

static void
srfi18_deallocate(vm_obj_t *obj)
{
  VM_FREE(obj->value.ext_object->opaque_data);
}

static void
srfi18_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_srfi18_exception_t *e = obj->value.ext_object->opaque_data;
  vm_write(port, "#<%s>", srfi18_name(e->kind));
}

static void
srfi18_mark(vm_obj_t *obj)
{
  vm_srfi18_exception_t *e = obj->value.ext_object->opaque_data;
  vm_gc_mark_pointer(e);
  /* The reason slot may hold a heap-allocated string or any other
     ref-typed value; walk it so it survives as long as the wrapper
     does. For kinds without a reason this is a VM_TYPE_NONE no-op. */
  vm_gc_mark_object(&e->reason);
}

int
vm_srfi18_exception_check_type(const vm_obj_t *obj)
{
  return obj->type == VM_TYPE_EXTERNAL &&
         obj->value.ext_object != NULL &&
         obj->value.ext_object->type == &ext_type_srfi18_exception;
}

vm_srfi18_exception_kind_t
vm_srfi18_exception_kind(const vm_obj_t *obj)
{
  vm_srfi18_exception_t *e = obj->value.ext_object->opaque_data;
  return e->kind;
}

void
vm_srfi18_exception_get_reason(const vm_obj_t *obj, vm_obj_t *out)
{
  vm_srfi18_exception_t *e;

  out->type = VM_TYPE_NONE;
  if(!vm_srfi18_exception_check_type(obj)) {
    return;
  }
  e = obj->value.ext_object->opaque_data;
  if(e->kind == VM_SRFI18_UNCAUGHT_EXCEPTION) {
    memcpy(out, &e->reason, sizeof(vm_obj_t));
  }
}

/* Allocate a SRFI 18 exception external. Returns 0 on heap failure. */
static int
srfi18_build(vm_obj_t *dst, vm_srfi18_exception_kind_t kind,
             const vm_obj_t *reason)
{
  vm_srfi18_exception_t *e;

  e = VM_MALLOC(sizeof(vm_srfi18_exception_t));
  if(e == NULL) {
    return 0;
  }
  e->kind = kind;
  if(reason != NULL && kind == VM_SRFI18_UNCAUGHT_EXCEPTION) {
    memcpy(&e->reason, reason, sizeof(vm_obj_t));
  } else {
    e->reason.type = VM_TYPE_NONE;
  }
  if(vm_ext_object_create(dst, &ext_type_srfi18_exception, e) == NULL) {
    VM_FREE(e);
    return 0;
  }
  return 1;
}

void
vm_raise_srfi18_exception(vm_thread_t *thread,
                          vm_srfi18_exception_kind_t kind,
                          const vm_obj_t *reason)
{
  vm_obj_t obj;

  if(!srfi18_build(&obj, kind, reason)) {
    /* Heap exhausted -- fall back to a generic thread error so
       something visible happens; the caller's guard cannot catch
       it but the alternative is to lose the failure entirely. */
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  vm_raise_exception(thread, &obj);
}

VM_FUNCTION(join_timeout_exceptionp)
{
  VM_PUSH_BOOLEAN(vm_srfi18_exception_check_type(&argv[0]) &&
                  vm_srfi18_exception_kind(&argv[0]) ==
                    VM_SRFI18_JOIN_TIMEOUT);
}

VM_FUNCTION(abandoned_mutex_exceptionp)
{
  VM_PUSH_BOOLEAN(vm_srfi18_exception_check_type(&argv[0]) &&
                  vm_srfi18_exception_kind(&argv[0]) ==
                    VM_SRFI18_ABANDONED_MUTEX);
}

VM_FUNCTION(terminated_thread_exceptionp)
{
  VM_PUSH_BOOLEAN(vm_srfi18_exception_check_type(&argv[0]) &&
                  vm_srfi18_exception_kind(&argv[0]) ==
                    VM_SRFI18_TERMINATED_THREAD);
}

VM_FUNCTION(uncaught_exceptionp)
{
  VM_PUSH_BOOLEAN(vm_srfi18_exception_check_type(&argv[0]) &&
                  vm_srfi18_exception_kind(&argv[0]) ==
                    VM_SRFI18_UNCAUGHT_EXCEPTION);
}

VM_FUNCTION(uncaught_exception_reason)
{
  if(!vm_srfi18_exception_check_type(&argv[0]) ||
     vm_srfi18_exception_kind(&argv[0]) !=
       VM_SRFI18_UNCAUGHT_EXCEPTION) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  vm_srfi18_exception_get_reason(&argv[0], &thread->result);
}
