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
 */

#ifndef VM_PROFILER_H
#define VM_PROFILER_H

#include "vm.h"

/* Internal VM profiling functionality. */

#if VM_PROFILER_ENABLE
#define VM_PROFILER_CALL_START(op_id)       vm_profiler_call_start(op_id)
#define VM_PROFILER_CALL_END(op_id)         vm_profiler_call_end(op_id)
#define VM_PROFILER_INVOCATION_START(op_id) vm_profiler_invocation_start(op_id)
#define VM_PROFILER_INVOCATION_END(op_id)   vm_profiler_invocation_end(op_id)
#else
#define VM_PROFILER_CALL_START(op_id)
#define VM_PROFILER_CALL_END(op_id)
#define VM_PROFILER_INVOCATION_START(op_id)
#define VM_PROFILER_INVOCATION_END(op_id)
#endif /* VM_PROFILER_ENABLE */

struct vm_profiler_op_stats {
  uint32_t invocations;
  uint32_t calls;
  vm_native_time_t min_call_time;
  vm_native_time_t max_call_time;
  vm_native_time_t min_invocation_time;
  vm_native_time_t max_invocation_time;
  uint32_t total_call_time;
  uint32_t total_invocation_time;
};

int vm_profiler_init(void);
void vm_profiler_call_start(vm_symbol_id_t);
void vm_profiler_call_end(vm_symbol_id_t);
void vm_profiler_invocation_start(vm_symbol_id_t);
void vm_profiler_invocation_end(vm_symbol_id_t);
void vm_profiler_print_stats(void);

#endif /* !VM_PROFILER_H */
