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
 * Author: Nicolas Tsiftes <nicolas.tsiftes@ri.se>
 */

#ifndef VM_REPL_H
#define VM_REPL_H

#ifdef VM_REPL_ENABLE

#include "vm.h"

/*
 * VeloxVM interactive REPL service.
 *
 * The REPL service maintains a single live vm_program_t (the "REPL
 * program") whose tables grow as deltas arrive from the host-side
 * compiler. See doc/repl-design.md for the architecture, the wire
 * format, and the sync invariants this API implements.
 *
 * All functions return non-zero on success, zero on failure, except
 * where noted.
 */

/* Result codes for vm_repl_apply_delta. Negative values indicate
   irrecoverable errors that should desync the session. */
typedef enum {
  VM_REPL_OK              =  0,   /* delta applied; out_entry_id valid */
  VM_REPL_DUPLICATE       =  1,   /* delta already seen; out_entry_id valid */
  VM_REPL_BAD_MAGIC       = -1,
  VM_REPL_BAD_VERSION     = -2,
  VM_REPL_BAD_FORMAT      = -3,
  VM_REPL_OUT_OF_SYNC     = -4,   /* start_id mismatch */
  VM_REPL_OUT_OF_MEMORY   = -5,
  VM_REPL_LIMIT_EXCEEDED  = -6
} vm_repl_status_t;

/*
 * Create a fresh REPL program, register it with the VM, and return it.
 * The program starts with empty growable tables and a single no-op
 * placeholder expression at index 0. policy is optional; pass NULL to
 * use the default REPL policy.
 */
vm_program_t *vm_repl_program_create(const char *name,
                                     const vm_policy_t *policy);

/*
 * Apply a delta to the REPL program. delta_bytes points at the start
 * of the wire-format payload (magic 0x5E 0xB6 ...). On success, sets
 * *out_entry_id to the entry expression id read from the ENTRY_EXPR
 * section.
 */
vm_repl_status_t vm_repl_apply_delta(vm_program_t *program,
                                     const uint8_t *delta_bytes,
                                     size_t delta_len,
                                     vm_expr_id_t *out_entry_id);

/*
 * Synchronous run: redirects the parked main thread at entry_id,
 * drives vm_run() until the thread parks again, returns the result.
 * Suitable for hosted ports (POSIX) where blocking is fine.
 *
 * Returns 1 on success with *out_result initialized. Returns 0 on
 * error with *out_error initialized.
 */
int vm_repl_run(vm_program_t *program,
                vm_expr_id_t entry_id,
                vm_obj_t *out_result,
                vm_error_t *out_error);

/*
 * Cooperative variant: redirect the main thread and return
 * immediately. Lets the host process loop drive vm_run() at its own
 * cadence (e.g., a Contiki process that must yield between vm_run
 * invocations to keep the CoAP stack alive). Returns 1 on successful
 * setup, 0 on error.
 */
int vm_repl_start(vm_program_t *program,
                  vm_expr_id_t entry_id,
                  vm_error_t *out_error);

/*
 * Cooperative companion to vm_repl_start. Probes the main thread:
 *
 *   1   thread parked; *out_result populated (caller may also
 *       call vm_repl_encode_obj to serialize for transit)
 *   0   thread still running; nothing emitted
 *  -1   thread errored; *out_error populated
 *
 * The caller should call vm_run() between vm_repl_collect() polls,
 * yielding to the host event loop in between.
 */
int vm_repl_collect(vm_program_t *program,
                    vm_obj_t *out_result,
                    vm_error_t *out_error);

/*
 * Tear down a REPL program. Removes it from the loaded-programs list,
 * destroys any threads bound to it, and frees its tables (including
 * freestanding items added via the append-loader).
 */
void vm_repl_program_destroy(vm_program_t *program);

/*
 * Encode obj into the runtime value wire format. Returns the number of
 * bytes written, or 0 if cap is too small. Implementation in
 * core/repl/vm-repl-encode.c.
 */
size_t vm_repl_encode_obj(vm_program_t *program, const vm_obj_t *obj,
                          uint8_t *out, size_t cap);

#endif /* VM_REPL_ENABLE */

#endif /* !VM_REPL_H */
