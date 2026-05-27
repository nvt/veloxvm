/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES ARE DISCLAIMED.
 */

#ifndef VM_EXCEPTIONS_H
#define VM_EXCEPTIONS_H

#include "vm.h"

/* SRFI 18 condition types that the runtime raises directly. User
   code dispatches on these inside a guard clause via the matching
   predicate primitive. */
typedef enum vm_srfi18_exception_kind {
  VM_SRFI18_JOIN_TIMEOUT       = 0,
  VM_SRFI18_ABANDONED_MUTEX    = 1,
  VM_SRFI18_TERMINATED_THREAD  = 2,
  VM_SRFI18_UNCAUGHT_EXCEPTION = 3
} vm_srfi18_exception_kind_t;

/* Raise a SRFI 18 typed exception in `thread`'s context. For
   VM_SRFI18_UNCAUGHT_EXCEPTION, `reason` is the original exception
   the joinee did not catch; for the other kinds, pass NULL. Walks
   `thread`'s frame stack to find an enclosing guard and jumps to
   its handler; if no guard is reachable, transitions the thread to
   VM_THREAD_ERROR. May be called from a different thread's
   scheduler tick (e.g. by vm_thread_finalize_joiners or a wake-up
   callback) -- it manipulates only `thread`'s state. */
void vm_raise_srfi18_exception(vm_thread_t *thread,
                               vm_srfi18_exception_kind_t kind,
                               const vm_obj_t *reason);

/* Returns truthy iff obj is a SRFI 18 typed-exception external. */
int vm_srfi18_exception_check_type(const vm_obj_t *obj);

/* Reads the kind tag. Caller is responsible for verifying the
   object is a SRFI 18 exception first. */
vm_srfi18_exception_kind_t
vm_srfi18_exception_kind(const vm_obj_t *obj);

/* For VM_SRFI18_UNCAUGHT_EXCEPTION: copy the wrapped reason into
   *out. For other kinds (or non-exception args), writes a
   VM_TYPE_NONE sentinel. */
void vm_srfi18_exception_get_reason(const vm_obj_t *obj, vm_obj_t *out);

#endif /* !VM_EXCEPTIONS_H */
