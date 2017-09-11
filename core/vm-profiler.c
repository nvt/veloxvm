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

#include "vm-profiler.h"
#include "vm-log.h"

/* An array of statistics structures for each core VM operation. */
static struct vm_profiler_op_stats *op_stats_array;

/* Store the maximum operator ID for sanity checks in the functions below. */
static vm_symbol_id_t max_op_id;

/* Internal profiler state. */
static vm_native_time_t call_start;
static vm_native_time_t invocation_start;
static vm_symbol_id_t current_op_id;

int
vm_profiler_init(void)
{
  unsigned op_count;

  op_count = vm_procedure_count();
  max_op_id = op_count - 1;
  op_stats_array = VM_MALLOC(sizeof(*op_stats_array) * op_count);
  if(op_stats_array == NULL) {
    return 0;
  }

  memset(op_stats_array, 0, sizeof(*op_stats_array) * op_count);

  VM_DEBUG(VM_DEBUG_LOW, "Initialized the VM profiler for %u operations",
           op_count);

  return 1;
}

void
vm_profiler_call_start(vm_symbol_id_t op_id)
{
  if(op_id > max_op_id) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Profile operator with invalid ID %u",
	     (unsigned)op_id);
    return;
  }

  call_start = VM_NATIVE_TIME();
  current_op_id = op_id;
}

void
vm_profiler_call_end(vm_symbol_id_t op_id)
{
  vm_native_time_t call_end, diff;
  struct vm_profiler_op_stats *op_stats;

  call_end = VM_NATIVE_TIME();

  if(op_id != current_op_id) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Profile operator with invalid ID %u",
	     (unsigned)op_id);
    return;
  }

  op_stats = &op_stats_array[op_id];

  op_stats->calls++;
  diff = call_end - call_start;

  if(diff < op_stats->min_call_time ||
     op_stats->min_call_time == 0) {
    op_stats->min_call_time = diff;
  }

  if(diff > op_stats->max_call_time) {
    op_stats->max_call_time = diff;
  }
}

void
vm_profiler_invocation_start(vm_symbol_id_t op_id)
{
  current_op_id = op_id; /* Temporary fix. */

  if(op_id != current_op_id) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Profile operator with invalid ID %u",
	     (unsigned)op_id);
    return;
  }

  invocation_start = VM_NATIVE_TIME();
}

void
vm_profiler_invocation_end(vm_symbol_id_t op_id)
{
  vm_native_time_t invocation_end, diff;
  struct vm_profiler_op_stats *op_stats;

  invocation_end = VM_NATIVE_TIME();

  if(op_id != current_op_id) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Profile operator with invalid ID %u",
	     (unsigned)op_id);
    return;
  }

  op_stats = &op_stats_array[op_id];

  op_stats->invocations++;
  diff = invocation_end - invocation_start;

  if(diff < op_stats->min_invocation_time ||
     op_stats->min_invocation_time == 0) {
    op_stats->min_invocation_time = diff;
  }

  if(diff > op_stats->max_invocation_time) {
    op_stats->max_invocation_time = diff;
  }
}

void
vm_profiler_print_stats(void)
{
  unsigned i;
  struct vm_profiler_op_stats *op_stats;
  vm_symbol_ref_t ref;
  const char *name;

  VM_DEBUG(VM_DEBUG_LOW, "VM profiler dump (time resolution %lu Hz)",
	   (unsigned long)VM_NATIVE_TIME_RESOLUTION());

  ref.scope = VM_SYMBOL_SCOPE_CORE;

  for(i = 0; i <= max_op_id; i++) {
    op_stats = op_stats_array + i;
    if(op_stats->invocations > 0) {
      ref.symbol_id = i;
      name = vm_symbol_lookup(NULL, &ref);
      VM_DEBUG(VM_DEBUG_LOW, "Op %-10s ID %2u C %lu I %lu min_I_time %u max_I_time %u min_C_time %lu max_C_time %lu",
               name != NULL ? name : "(?)", i,
               (unsigned long)op_stats->calls,
               (unsigned long)op_stats->invocations,
               (unsigned long)op_stats->min_invocation_time,
               (unsigned long)op_stats->max_invocation_time,
               (unsigned long)op_stats->min_call_time,
               (unsigned long)op_stats->max_call_time);
    }
  }
}
