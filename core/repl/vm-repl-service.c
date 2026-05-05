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

/*
 * REPL service: lifecycle for the live REPL program, plus
 * vm_repl_run, which redirects the parked main thread at a new
 * entry expression, drives the scheduler until the thread parks
 * again or errors out, and surfaces the result/error via
 * vm_repl_collect.
 */

#ifdef VM_REPL_ENABLE

#include "vm.h"
#include "vm-log.h"
#include "vm-native.h"
#include "vm-repl.h"
#include "vm-table.h"

#include <string.h>

vm_program_t *
vm_repl_program_create(const char *name, const vm_policy_t *policy)
{
  vm_program_t *program;
  size_t name_len;

  program = VM_MALLOC(sizeof(*program));
  if(program == NULL) {
    return NULL;
  }
  memset(program, 0, sizeof(*program));

  name_len = strlen(name);
  program->name = VM_MALLOC(name_len + 1);
  if(program->name == NULL) {
    goto fail;
  }
  memcpy(program->name, name, name_len + 1);

  if(!vm_table_init_growable(&program->strings) ||
     !vm_table_init_growable(&program->symbols) ||
     !vm_table_init_growable(&program->exprv)) {
    goto fail;
  }

  program->captures = NULL;
  program->captures_size = 0;
  program->symbol_bindings = NULL;

  if(!vm_loader_register_program(program)) {
    goto fail;
  }

  /* vm_loader_register_program calls vm_policy_init_program which sets
     the policy. Override only if the caller supplied a non-NULL policy
     explicitly. */
  if(policy != NULL) {
    program->policy = policy;
  }

  /* Create the main REPL thread once. It starts parked with no entry
     bound; vm_repl_run sets its ip/end and unparks it on each turn. */
  if(vm_thread_create_parked(program) == NULL) {
    vm_unload_program(program);
    return NULL;
  }

  return program;

 fail:
  if(program != NULL) {
    if(program->name != NULL) {
      VM_FREE(program->name);
    }
    vm_table_destroy(&program->strings);
    vm_table_destroy(&program->symbols);
    vm_table_destroy(&program->exprv);
    VM_FREE(program);
  }
  return NULL;
}

void
vm_repl_program_destroy(vm_program_t *program)
{
  if(program != NULL) {
    /* vm_unload_program walks loaded_programs, unlinks our entry,
       destroys all threads bound to programs, and calls free_program
       which in turn calls vm_table_destroy on each table -- which
       knows about freestanding items via the VM_REPL_ENABLE-gated
       extension. So a single call covers REPL teardown. */
    vm_unload_program(program);
  }
}

/* Locate the REPL main thread bound to this program. */
static vm_thread_t *
find_main_thread(vm_program_t *program)
{
  unsigned i;
  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    vm_thread_t *t = vm_thread_get_by_index(i);
    if(t != NULL && t->program == program && t->repl_main) {
      return t;
    }
  }
  return NULL;
}

int
vm_repl_start(vm_program_t *program, vm_expr_id_t entry_id,
              vm_error_t *out_error)
{
  vm_thread_t *thread;
  vm_expr_t *expr;
  uint8_t *bytecode;
  unsigned bytecode_len;

  if(program == NULL ||
     entry_id >= (vm_expr_id_t)VM_TABLE_SIZE(program->exprv)) {
    if(out_error != NULL) {
      out_error->error_type = VM_ERROR_EXPR_ID;
      out_error->error_obj.type = VM_TYPE_NONE;
    }
    return 0;
  }

  thread = find_main_thread(program);
  if(thread == NULL) {
    if(out_error != NULL) {
      out_error->error_type = VM_ERROR_THREAD;
      out_error->error_obj.type = VM_TYPE_NONE;
    }
    return 0;
  }

  /* If the previous turn left the thread in any state other than
     PARKED (e.g. WAITING for a mutex/sleep that never returned, or
     ERROR), refuse: the driver should reset. */
  if(thread->status != VM_THREAD_PARKED) {
    if(out_error != NULL) {
      out_error->error_type = VM_ERROR_THREAD;
      out_error->error_obj.type = VM_TYPE_NONE;
    }
    return 0;
  }

  bytecode = VM_TABLE_GET(program->exprv, entry_id);
  bytecode_len = VM_TABLE_LENGTH(program->exprv, entry_id);

  /* Redirect the parked top frame at the new entry expression and
     reset the per-form scratch state. The frame stack is at exprc==1
     because the previous turn unwound back to the top (or this is
     the first turn). */
  expr = thread->expr;
  expr->ip = bytecode;
  expr->end = bytecode + bytecode_len;
  expr->flags = 0;
  expr->argc = 0;
  expr->bindc = 0;
  expr->eval_completed = 0;
  expr->eval_requested = 0;
  expr->eval_arg = 0;
  expr->procedure = NULL;
  expr->expr_id = entry_id;

  thread->result.type = VM_TYPE_NONE;
  thread->error.error_obj.type = VM_TYPE_NONE;
  thread->error.error_type = VM_ERROR_INTERNAL;
  thread->status = VM_THREAD_RUNNABLE;

  return 1;
}

/* Tear the main thread's evaluation frame stack back down to a single
   top frame so the next vm_repl_start has a clean slate. After a
   normal park the stack is already at exprc==1, but after an error
   it can be deep in a sub-frame. */
static void
reset_main_thread_to_parked(vm_thread_t *thread)
{
  while(thread->exprc > 1) {
    vm_thread_stack_pop(thread);
  }
  if(thread->expr != NULL) {
    thread->expr->flags = 0;
    thread->expr->argc = 0;
    thread->expr->bindc = 0;
    thread->expr->eval_completed = 0;
    thread->expr->eval_requested = 0;
    thread->expr->eval_arg = 0;
    thread->expr->procedure = NULL;
    thread->expr->ip = NULL;
    thread->expr->end = NULL;
  }
  thread->error.error_obj.type = VM_TYPE_NONE;
  thread->error.error_type = VM_ERROR_INTERNAL;
  thread->error.repl_error_printed = 0;
  thread->status = VM_THREAD_PARKED;
}

int
vm_repl_collect(vm_program_t *program, vm_obj_t *out_result,
                vm_error_t *out_error)
{
  vm_thread_t *thread = find_main_thread(program);
  if(thread == NULL) {
    if(out_error != NULL) {
      out_error->error_type = VM_ERROR_THREAD;
      out_error->error_obj.type = VM_TYPE_NONE;
    }
    return -1;
  }

  if(thread->status == VM_THREAD_PARKED) {
    if(out_result != NULL) {
      *out_result = thread->result;
    }
    return 1;
  }
  if(thread->status == VM_THREAD_ERROR ||
     thread->status == VM_THREAD_EXITING ||
     thread->status == VM_THREAD_FINISHED) {
    if(out_error != NULL) {
      *out_error = thread->error;
    }
    /* Recover so the next REPL turn can run on the same thread. */
    reset_main_thread_to_parked(thread);
    return -1;
  }
  return 0;
}

int
vm_repl_run(vm_program_t *program, vm_expr_id_t entry_id,
            vm_obj_t *out_result, vm_error_t *out_error)
{
  if(!vm_repl_start(program, entry_id, out_error)) {
    return 0;
  }

  for(;;) {
    vm_result_t r = vm_run();
    int status = vm_repl_collect(program, out_result, out_error);
    if(status == 1) {
      return 1;
    }
    if(status < 0) {
      return 0;
    }
    /* If the scheduler reports SLEEPING, a thread is in WAITING --
       most commonly waiting on a thread-sleep! timer or a port. We
       must let the host's poll path service those waits or the
       timer never fires and the REPL turn hangs. vm_native_poll
       blocks until something becomes ready (timer expires, fd
       ready, etc.) and dispatches. */
    if(r == VM_RESULT_SLEEPING) {
      vm_native_poll();
      continue;
    }
    if(r == VM_RESULT_FINISHED) {
      /* Defensive: vm_run returned FINISHED but the thread is still
         RUNNABLE (per collect). Should not happen -- avoid an
         infinite loop. */
      if(out_error != NULL) {
        out_error->error_type = VM_ERROR_INTERNAL;
        out_error->error_obj.type = VM_TYPE_NONE;
      }
      return 0;
    }
  }
}

#endif /* VM_REPL_ENABLE */
