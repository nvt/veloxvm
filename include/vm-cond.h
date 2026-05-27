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

#ifndef VM_COND_H
#define VM_COND_H

#include "vm.h"

/* Returns truthy iff obj is the external-object form returned by
   make-condition-variable. */
int vm_cond_check_type(const vm_obj_t *obj);

/* Atomic unlock-and-wait support for mutex-unlock! mutex cv [timeout].
   Adds `thread` to cv_obj's wait list, registers a wait_cancel hook
   so a racing timeout can pull self back out, and (if timeout_ms is
   non-negative) schedules a wake timer through vm_native_sleep. The
   caller (op_mutex_unlock) is responsible for releasing the mutex
   first and for setting thread->status = VM_THREAD_WAITING after
   this returns; on failure (heap allocation) the function calls
   vm_signal_error and returns 0. */
int vm_cond_park_thread(vm_obj_t *cv_obj, vm_thread_t *thread,
                        vm_integer_t timeout_ms);

#endif /* !VM_COND_H */
