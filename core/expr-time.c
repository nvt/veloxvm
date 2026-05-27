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
 * SRFI-18 time objects. A time object encodes an absolute point in
 * time as seconds + milliseconds since the Unix epoch -- the same
 * representation vm_native_time returns. The primary use is as the
 * timeout argument to thread-sleep!, thread-join!, mutex-lock!, and
 * mutex-unlock! when the caller wants a deadline-based wait rather
 * than the VeloxVM integer-ms-relative convention.
 */

#include "vm-functions.h"
#include "vm-time.h"
#include "vm-native.h"

typedef struct vm_time_obj {
  vm_time_t t;
} vm_time_obj_t;

static void time_copy(vm_obj_t *, vm_obj_t *);
static void time_deallocate(vm_obj_t *);
static void time_write(vm_port_t *, vm_obj_t *);
static void time_mark(vm_obj_t *);

static vm_ext_type_t ext_type_time = {
  .copy = time_copy,
  .deallocate = time_deallocate,
  .write = time_write,
  .mark = time_mark
};

static void
time_copy(vm_obj_t *dst, vm_obj_t *src)
{
  /* Time objects are immutable value types; sharing the box is the
     same as duplicating, and cheaper. */
  memcpy(dst, src, sizeof(vm_obj_t));
}

static void
time_deallocate(vm_obj_t *obj)
{
  VM_FREE(obj->value.ext_object->opaque_data);
}

static void
time_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_time_obj_t *t = obj->value.ext_object->opaque_data;
  vm_write(port, "#<time %lu.%03u>",
           (unsigned long)t->t.sec, (unsigned)t->t.msec);
}

static void
time_mark(vm_obj_t *obj)
{
  vm_time_obj_t *t = obj->value.ext_object->opaque_data;
  vm_gc_mark_pointer(t);
}

int
vm_time_check_type(const vm_obj_t *obj)
{
  return obj->type == VM_TYPE_EXTERNAL &&
         obj->value.ext_object != NULL &&
         obj->value.ext_object->type == &ext_type_time;
}

/* Allocate a heap-backed time external initialised from `t`. */
static int
push_time(vm_obj_t *dst, vm_time_t t)
{
  vm_time_obj_t *box;

  box = VM_MALLOC(sizeof(vm_time_obj_t));
  if(box == NULL) {
    return 0;
  }
  box->t = t;
  if(vm_ext_object_create(dst, &ext_type_time, box) == NULL) {
    VM_FREE(box);
    return 0;
  }
  return 1;
}

vm_integer_t
vm_time_parse_timeout(const vm_obj_t *obj, int *valid)
{
  vm_time_obj_t *box;
  vm_time_t now;
  int64_t delta_ms;

  if(valid != NULL) {
    *valid = 1;
  }
  if(obj->type == VM_TYPE_INTEGER) {
    /* VeloxVM convention: integers are relative milliseconds. This
       diverges from SRFI 18, which interprets a number as absolute
       seconds since the epoch, but preserves the meaning of existing
       calls like (thread-sleep! 100). */
    return obj->value.integer;
  }
  if(obj->type == VM_TYPE_BOOLEAN) {
    /* #f (or any boolean) is "no timeout". */
    return -1;
  }
  if(vm_time_check_type(obj)) {
    box = obj->value.ext_object->opaque_data;
    if(!vm_native_time(&now)) {
      if(valid != NULL) {
        *valid = 0;
      }
      return -1;
    }
    delta_ms = ((int64_t)box->t.sec - (int64_t)now.sec) * 1000 +
               ((int64_t)box->t.msec - (int64_t)now.msec);
    if(delta_ms <= 0) {
      return 0;
    }
    if(delta_ms > INT32_MAX) {
      return INT32_MAX;
    }
    return (vm_integer_t)delta_ms;
  }
  if(valid != NULL) {
    *valid = 0;
  }
  return -1;
}

VM_FUNCTION(current_time)
{
  vm_time_t now;

  if(!vm_native_time(&now)) {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
    return;
  }
  if(!push_time(&thread->result, now)) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  }
}

VM_FUNCTION(timep)
{
  VM_PUSH_BOOLEAN(vm_time_check_type(&argv[0]));
}

VM_FUNCTION(time_to_seconds)
{
  vm_time_obj_t *box;

  if(!vm_time_check_type(&argv[0])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  box = argv[0].value.ext_object->opaque_data;

  if(box->t.msec == 0) {
    /* Whole-second times round-trip as integers: (time->seconds
       (seconds->time 100)) returns 100, not 100.0. This keeps
       arithmetic and printing portable across targets where reals
       may not be enabled. */
    VM_PUSH_INTEGER((vm_integer_t)box->t.sec);
    return;
  }
#if VM_ENABLE_REALS
  /* Preserve sub-second precision when reals are available. */
  thread->result.type = VM_TYPE_REAL;
  thread->result.value.real = (vm_real_t)box->t.sec +
                              (vm_real_t)box->t.msec / 1000.0;
#else
  /* Embedded targets without reals get integer-second precision;
     the fractional ms is dropped. */
  VM_PUSH_INTEGER((vm_integer_t)box->t.sec);
#endif
}

VM_FUNCTION(seconds_to_time)
{
  vm_time_t t;

  switch(argv[0].type) {
  case VM_TYPE_INTEGER:
    if(argv[0].value.integer < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }
    t.sec = (uint32_t)argv[0].value.integer;
    t.msec = 0;
    break;
  case VM_TYPE_RATIONAL: {
    /* (num * 1000) / denom gives total milliseconds; split into
       sec / msec. Stay in int64 to avoid intermediate overflow
       on typical Unix-epoch seconds. */
    int64_t num = argv[0].value.rational->numerator;
    int64_t denom = argv[0].value.rational->denominator;
    int64_t ms;
    if(denom == 0 || num < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }
    ms = (num * 1000) / denom;
    t.sec = (uint32_t)(ms / 1000);
    t.msec = (uint16_t)(ms % 1000);
    break;
  }
#if VM_ENABLE_REALS
  case VM_TYPE_REAL: {
    vm_real_t s = argv[0].value.real;
    if(s < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }
    t.sec = (uint32_t)s;
    t.msec = (uint16_t)((s - (vm_real_t)t.sec) * 1000.0);
    break;
  }
#endif
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  if(!push_time(&thread->result, t)) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  }
}
