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

#include <stdio.h>
#include <stdlib.h>

#include "vm-bytecode.h"
#include "vm-functions.h"
#include "vm-log.h"
#include "vm-native.h"

/*
 * This variable makes it possible to determine which thread is
 * currently executing, and thus removes any need to pass thread
 * information in deep call chains.
 */
static vm_thread_t *current_thread;

void
vm_thread_set_expr(vm_thread_t *thread, vm_expr_id_t expr_id)
{
  vm_expr_t *expr;

  if(expr_id >= VM_TABLE_SIZE(thread->program->exprv)) {
    vm_signal_error(thread, VM_ERROR_BYTECODE);
    return;
  }

  expr = thread->expr;

  expr->expr_id = expr_id;
  expr->ip = VM_TABLE_GET(thread->program->exprv, expr_id);
  expr->end = expr->ip + VM_TABLE_LENGTH(thread->program->exprv, expr_id);
}

static void
cut_tail_call_frames(vm_thread_t *thread)
{
  vm_expr_id_t lambda_expr_id;
  int i;
  int j;

  lambda_expr_id = thread->expr->expr_id;

  for(i = thread->exprc - 2; i > 0; i--) {
    if(thread->exprv[i]->expr_id == lambda_expr_id) {
      for(j = thread->exprc - 2; j > i; j--) {
        if(IS_CLEAR(thread->exprv[j]->flags, VM_EXPR_TAIL_CALL)) {
          /* Unable to optimize the tail because one of the expressions
             in the stack frames under consideration is not a tail call. */
          return;
        }
      }

      /* Reduce the stack by thread->exprc - 1 - i frames. */
      for(j = thread->exprc - 2; j >= i; j--) {
        vm_thread_stack_free(thread->exprv[j]);
      }
      thread->exprv[i] = thread->exprv[thread->exprc - 1];
      thread->exprc = i + 1;
      thread->expr = thread->exprv[i];
      return;
    }
  }
}

static void
init_lambda_execution(vm_thread_t *thread, vm_expr_t *expr)
{
  VM_EVAL_SET_REQUESTED_RANGE(thread, 1, expr->argc);

  SET(expr->flags, VM_EXPR_LAMBDA);
  if(thread->exprc >= 2 &&
     IS_SET(thread->exprv[thread->exprc - 2]->flags, VM_EXPR_TAIL_CALL)) {
    /* If the calling expression is a tail call, then so is the next
       lambda expression to be called. */
    SET(expr->flags, VM_EXPR_TAIL_CALL);
  }
}

static void
vm_sched_thread(vm_thread_t *thread)
{
  vm_expr_t *expr;
  vm_obj_t next_expr;
  int i;

  thread->stats.schedulings++;
  expr = thread->expr;

  if(IS_SET(expr->flags, VM_EXPR_RESTART)) {
    goto restart;
  }

  if(IS_CLEAR(expr->flags, VM_EXPR_HAVE_OBJECTS)) {
    /* We are at the start of a new expression;
       get an expression header or an atom. */
    vm_get_object(thread, &next_expr);
    if(thread->status == VM_THREAD_ERROR) {
      return;
    }

    if(next_expr.type == VM_TYPE_FORM) {
      /* We are reading an expression that consists of a form. The bytecode
         header tells us how many objects to expect to read. */
      expr->argc = next_expr.value.form.argc;

      /*
       * Ensure that the number of function call arguments specified in
       * the bytecode does not exceed the maximum number of objects that
       * can be stored in a stack frame.
       */
      if(expr->argc > VM_OBJECT_STACK_SIZE) {
        vm_signal_error(thread, VM_ERROR_STACK_OVERFLOW);
        vm_set_error_string(thread, "too many arguments");
        return;
      }

      /* Force evaluation of the first object, to determine which internal
         operator or external lambda function to call. */
      expr->eval_requested = 1;

      /* Load all objects that are part of the expr. */
      for(i = 0; i < expr->argc && expr->ip < expr->end; i++) {
        vm_get_object(thread, &expr->argv[i]);
        if(thread->status == VM_THREAD_ERROR) {
          VM_DEBUG(VM_DEBUG_LOW, "Failed to get object %d of expr %u",
                   i, (unsigned)expr->expr_id);
          return;
        }
#if 1
        if(i == 0 &&
           expr->argv[0].type == VM_TYPE_FORM &&
           expr->argv[0].value.form.type == VM_FORM_LAMBDA) {
          /*
           * If the first object is a lambda expression, then we
           * set a flag to indicate that the current expression
           * should execute in a special manner.
           */
          init_lambda_execution(thread, expr);
        }
#endif
      }
    } else {
      /* We read an expression consisting of an atom. */
      expr->argc = 0;
      vm_eval_object(thread, &next_expr);
      memmove(&expr->argv[0], &next_expr, sizeof(vm_obj_t));
    }

    SET(expr->flags, VM_EXPR_HAVE_OBJECTS);
  }

  do {
    /* Make the requested evaluations of arguments. */
    for(i = 0; i < expr->argc; i++) {
      if(VM_EVAL_REQUESTED(thread, i) && !VM_EVAL_COMPLETED(thread, i)) {
        expr->eval_arg = i;
        if(expr->argv[i].type == VM_TYPE_FORM) {
          if(i > 0 && expr->argv[i].value.form.type == VM_FORM_LAMBDA) {
            /* Lambda form arguments are evaluated to themselves. */
            VM_EVAL_SET_COMPLETED(thread, i);
            continue;
          }

          if(vm_thread_stack_push(thread) == NULL) {
            /* Attempted to allocate too many stack frames; stop the thread. */
            return;
          }

          /* Prepare the evaluation of the form argument, which will be
             executed next time when the scheduler invokes this thread. */
          vm_thread_set_expr(thread, expr->argv[i].value.form.id);
          return;
        } else {
          vm_eval_object(thread, &expr->argv[i]);
          if(thread->status == VM_THREAD_ERROR) {
            return;
          }

          if(i == 0 && expr->argv[0].type == VM_TYPE_FORM &&
             expr->argv[0].value.form.type == VM_FORM_LAMBDA) {
            /*
             * If the first object is a lambda expression, then we
             * set a flag to indicate that the current expression
             * should execute in a special manner.
             */
            init_lambda_execution(thread, expr);
            return;
          }
        }
        VM_EVAL_SET_COMPLETED(thread, i);
      }
    }

    /* Stack tail optimization. */
    if(IS_SET(expr->flags, VM_EXPR_LAMBDA)) {
      cut_tail_call_frames(thread);
    }

    if(expr->expr_id == 0) {
      SET(thread->expr->flags, VM_EXPR_TAIL_CALL);
    }

restart:
    vm_eval_expr(thread, expr);
    if(thread->status == VM_THREAD_ERROR ||
       thread->status == VM_THREAD_EXITING) {
      return;
    }
    /* A native function may have rewritten the expression stack,
       so the local expression pointer must be updated. */
    expr = thread->expr;
  } while(IS_CLEAR(expr->flags, VM_EXPR_LAMBDA) &&
          !VM_EVAL_COMPLETED_ALL(thread));

  if(IS_SET(expr->flags, VM_EXPR_LAMBDA) && !VM_EVAL_COMPLETED(thread, 0)) {
    VM_EVAL_SET_REQUESTED(thread, 0);
    return;
  }

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  vm_print_eval_expr(thread);
#endif

  if(IS_SET(expr->flags, VM_EXPR_RESTART)) {
    return;
  } else if(thread->exprc > 1) {
    /* Replace the evaluated object with the result of the evaluation. */
    vm_thread_stack_pop(thread);
    expr = thread->expr;
    VM_EVAL_SET_COMPLETED(thread, expr->eval_arg);
    memmove(&expr->argv[expr->eval_arg], &thread->result, sizeof(vm_obj_t));
  } else if(expr->ip == expr->end) {
    /* We have executed the last instruction of the top-level expr; the program
       is therefore finished. */
    thread->status = VM_THREAD_FINISHED;
  } else {
    /* Prepare execution of the next expression in the top-level expr. */
    expr->flags = 0;
    expr->argc = 0;
    expr->eval_completed = 0;
    expr->eval_requested = 0;
    expr->eval_arg = 0;
  }
}

vm_thread_t *
vm_current_thread(void)
{
  return current_thread;
}

/*
 * If at least one thread is runnable after scheduling it below, this
 * function will return VM_RESULT_RUNNING. By returning this value,
 * the VM suggests to the caller (typically the native application
 * implemented by each VM port) that the VM wants to execute again as
 * soon as possible.
 *
 * If there are no runnable threads, but at least one waiting thread,
 * the result will be VM_RESULT_SLEEPING. This is merely a hint to the
 * caller that it can put itself into sleep mode, as provided by the
 * host OS, until an event arrives for any of the VM threads. It is
 * also possible to call vm_run() again, but at the cost of using more
 * CPU time than necessary.
 *
 * Otherwise, the function will return VM_RESULT_FINISHED, indicating
 * that the VM does not have any more work to do and can be terminated
 * by calling vm_exit().
 */

vm_result_t
vm_run(void)
{
  vm_result_t result;
  unsigned i, j;
  vm_thread_t *thread;
  vm_program_t *program;

  /* Check for external events that may have occured since the VM was
     scheduled last time. */
  vm_native_poll();

  /*
   * Try to reclaim unused memory in order to reduce the risk of
   * failed heap allocations for the threads that are about to be
   * scheduled. The GC algorithm will check that certain conditions
   * are fulfilled before it runs a full scan of the heap.
   */
  if(VM_GC_AGGRESSIVE) {
    vm_gc();
  }

  result = VM_RESULT_FINISHED;

  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    thread = vm_thread_get_by_index(i);
    if(thread == NULL) {
      continue;
    }

    program = thread->program;

    if(thread->status == VM_THREAD_RUNNABLE) {
      current_thread = thread;
      /* Execute any pending expression in the thread. */
      j = thread->program->perf_attr.exec_instr_per_invocation;
      vm_native_accounting_start(thread);
      do {
        vm_sched_thread(thread);
      } while(--j > 0 && thread->status == VM_THREAD_RUNNABLE);
      vm_native_accounting_stop(thread);
    }

    /* Check whether status entails that the thread should be destroyed. */
    switch(thread->status) {
    case VM_THREAD_RUNNABLE:
      result = VM_RESULT_RUNNING;
      break;
    case VM_THREAD_WAITING:
      if(result == VM_RESULT_FINISHED) {
        result = VM_RESULT_SLEEPING;
      }
      break;
    case VM_THREAD_ERROR:
      vm_print_error(thread);
      vm_thread_destroy(thread);
      if(program->nthreads == 0) {
        vm_unload_program(program);
      }
      break;
    case VM_THREAD_EXITING:
      vm_unload_program(program);
      break;
    case VM_THREAD_FINISHED:
      vm_thread_destroy(thread);
      if(program->nthreads == 0) {
        vm_unload_program(program);
      }
      break;
    default:
      break;
    }

    current_thread = NULL;
  }

  return result;
}
