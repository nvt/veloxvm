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
        if(VM_IS_CLEAR(thread->exprv[j]->flags, VM_EXPR_TAIL_CALL)) {
          /* Unable to optimize the tail because one of the expressions
             in the stack frames under consideration is not a tail call. */
          return;
        }
      }

      /* By the time a tail call reaches here, bind_function has already
       * executed and created a new binding frame above frame i with the
       * recursive call's parameter values. Save those values, cut the
       * stack back to frame i, and copy them onto the surviving frame
       * before continuing. Handles simple and mutual recursion. */
      int old_bind_frame_idx = -1;
      int k;
      vm_obj_t *saved_bindings = NULL;
      int num_saved_bindings = 0;

      /* Find the bind frame directly below i - this is the OLD binding */
      for(j = i - 1; j > 0; j--) {
        if(thread->exprv[j]->bindc > 0) {
          old_bind_frame_idx = j;
          break;
        }
      }

      /* Find and save the NEW binding values from the topmost bind frame.
       * The newest bindings are at or near the top of the stack. */
      if(old_bind_frame_idx >= 0) {
        /* Search from the top of the stack downward for a bind frame with
         * the same symbols as the old frame */
        for(j = thread->exprc - 1; j > i; j--) {
          if(thread->exprv[j]->bindc > 0 &&
             thread->exprv[j]->bindc == thread->exprv[old_bind_frame_idx]->bindc) {
            /* Check if symbols match */
            int matches = 1;
            for(k = 0; k < thread->exprv[old_bind_frame_idx]->bindc; k++) {
              if(thread->exprv[j]->bindv[k].symbol_id !=
                 thread->exprv[old_bind_frame_idx]->bindv[k].symbol_id) {
                matches = 0;
                break;
              }
            }
            if(matches) {
              /* Save these binding values before we free the frames */
              num_saved_bindings = thread->exprv[j]->bindc;
              saved_bindings = VM_MALLOC(sizeof(vm_obj_t) * num_saved_bindings);
              if(saved_bindings) {
                for(k = 0; k < num_saved_bindings; k++) {
                  memcpy(&saved_bindings[k],
                         &thread->exprv[j]->bindv[k].obj,
                         sizeof(vm_obj_t));
                }
              }
              break;
            }
          }
        }
      }

      /* Reduce the stack by thread->exprc - 1 - i frames. */
      for(j = thread->exprc - 2; j >= i; j--) {
        vm_thread_stack_free(thread->exprv[j]);
      }
      thread->exprv[i] = thread->exprv[thread->exprc - 1];
      thread->exprc = i + 1;
      thread->expr = thread->exprv[i];

      /* Restore the saved binding values to the old bind frame */
      if(saved_bindings != NULL && old_bind_frame_idx >= 0) {
        for(k = 0; k < num_saved_bindings; k++) {
          memcpy(&thread->exprv[old_bind_frame_idx]->bindv[k].obj,
                 &saved_bindings[k],
                 sizeof(vm_obj_t));
        }
        VM_FREE(saved_bindings);
      }

      /* Reset execution state for tail-optimized frame. The merged frame
         is about to re-execute its call-site bytecode from scratch; the
         LAMBDA marker reflects the prior dispatch and must be cleared so
         that init_lambda_execution can re-engage for direct calls and so
         that primitive-headed call sites (e.g. apply) finish their
         eval-arg processing instead of exiting the dispatch loop early. */
      VM_CLEAR_FLAG(thread->expr->flags,
                    VM_EXPR_HAVE_OBJECTS | VM_EXPR_LAMBDA);
      thread->expr->eval_completed = 0;
      thread->expr->eval_requested = 0;
      thread->expr->ip = VM_TABLE_GET(thread->program->exprv, thread->expr->expr_id);
      return;
    }
  }
}

static void
init_lambda_execution(vm_thread_t *thread, vm_expr_t *expr)
{
  VM_EVAL_SET_REQUESTED_RANGE(thread, 1, expr->argc);

  VM_SET_FLAG(expr->flags, VM_EXPR_LAMBDA);
  if(thread->exprc >= 2 &&
     VM_IS_SET(thread->exprv[thread->exprc - 2]->flags, VM_EXPR_TAIL_CALL)) {
    /* If the calling expression is a tail call, then so is the next
       lambda expression to be called. */
    VM_SET_FLAG(expr->flags, VM_EXPR_TAIL_CALL);
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

  if(VM_IS_SET(expr->flags, VM_EXPR_RESTART)) {
    goto restart;
  }

  if(VM_IS_CLEAR(expr->flags, VM_EXPR_HAVE_OBJECTS)) {
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
           ((expr->argv[0].type == VM_TYPE_FORM &&
             expr->argv[0].value.form.type == VM_FORM_LAMBDA) ||
            expr->argv[0].type == VM_TYPE_CLOSURE)) {
          /*
           * If the first object is a lambda expression or a closure,
           * set a flag to indicate that the current expression should
           * execute in a special manner.
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

    VM_SET_FLAG(expr->flags, VM_EXPR_HAVE_OBJECTS);
  }

  do {
    /* Make the requested evaluations of arguments. */
    for(i = 0; i < expr->argc; i++) {
      if(VM_EVAL_REQUESTED(thread, i) && !VM_EVAL_COMPLETED(thread, i)) {
        expr->eval_arg = i;
        if(expr->argv[i].type == VM_TYPE_FORM ||
           expr->argv[i].type == VM_TYPE_CLOSURE) {
          if(i > 0 &&
             ((expr->argv[i].type == VM_TYPE_FORM &&
               expr->argv[i].value.form.type == VM_FORM_LAMBDA) ||
              expr->argv[i].type == VM_TYPE_CLOSURE)) {
            /* A lambda form occurring mid-expression as an argument
               (e.g. inside (cons L1 L2)) evaluates to itself, but if
               the lambda has captured free variables it must be
               materialized as a closure here, while the enclosing
               bind frame's bindings are still on the stack. Without
               this hook, the lambda would be passed around as a
               plain VM_FORM_LAMBDA with no captures, and its later
               call would fail with "undefined symbol" on its free
               vars. */
            if(expr->argv[i].type == VM_TYPE_FORM &&
               expr->argv[i].value.form.type == VM_FORM_LAMBDA &&
               thread->program->captures != NULL &&
               expr->argv[i].value.form.id < thread->program->captures_size &&
               thread->program->captures[expr->argv[i].value.form.id] != NULL) {
              vm_captures_t *cap =
                thread->program->captures[expr->argv[i].value.form.id];
              vm_expr_id_t form_id = expr->argv[i].value.form.id;
              vm_obj_t closure_obj;
              vm_closure_t *closure;
              uint8_t k;
              closure = vm_closure_create(&closure_obj, form_id, 0, cap->count);
              if(closure == NULL) {
                vm_signal_error(thread, VM_ERROR_HEAP);
                return;
              }
              for(k = 0; k < cap->count; k++) {
                vm_symbol_ref_t ref;
                vm_obj_t *bound;
                ref.scope = VM_SYMBOL_SCOPE_APP;
                ref.symbol_id = cap->symbols[k];
                bound = vm_symbol_resolve(thread, &ref);
                if(bound != NULL) {
                  memcpy(&closure->captures[k], bound, sizeof(vm_obj_t));
                }
              }
              memcpy(&expr->argv[i], &closure_obj, sizeof(vm_obj_t));
            }
            VM_EVAL_SET_COMPLETED(thread, i);
            continue;
          }

          if(vm_thread_stack_push(thread) == NULL) {
            /* Attempted to allocate too many stack frames; stop the thread. */
            return;
          }

          /* Prepare the evaluation of the form/closure argument. The
             closure dispatches into its body via closure->form_id; a
             plain form dispatches via form.id. */
          if(expr->argv[i].type == VM_TYPE_CLOSURE) {
            vm_thread_set_expr(thread, expr->argv[i].value.closure->form_id);
          } else {
            vm_thread_set_expr(thread, expr->argv[i].value.form.id);
          }
          return;
        } else {
          vm_eval_object(thread, &expr->argv[i]);
          if(thread->status == VM_THREAD_ERROR) {
            return;
          }

          if(i == 0 &&
             ((expr->argv[0].type == VM_TYPE_FORM &&
               expr->argv[0].value.form.type == VM_FORM_LAMBDA) ||
              expr->argv[0].type == VM_TYPE_CLOSURE)) {
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
    if(VM_IS_SET(expr->flags, VM_EXPR_LAMBDA)) {
      cut_tail_call_frames(thread);
      if(VM_IS_CLEAR(thread->expr->flags, VM_EXPR_HAVE_OBJECTS)) {
        /* The cut collapsed frames and reset the surviving frame so it
           re-reads its call-site bytecode. Return to the scheduler so the
           next dispatch enters that frame fresh through the load path. */
        return;
      }
    }

    if(expr->expr_id == 0) {
      VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
    }

restart:
    /* Don't call vm_eval_expr on completed lambda frames - argv[0] may be corrupted
       with the return value, and trying to evaluate it causes crashes for symbols. */
    if(!(VM_IS_SET(expr->flags, VM_EXPR_LAMBDA) && VM_EVAL_COMPLETED_ALL(thread))) {
      vm_eval_expr(thread, expr);
      if(thread->status == VM_THREAD_ERROR ||
         thread->status == VM_THREAD_EXITING) {
        return;
      }
    }
    /* A native function may have rewritten the expression stack,
       so the local expression pointer must be updated. */
    expr = thread->expr;
  } while(VM_IS_CLEAR(expr->flags, VM_EXPR_LAMBDA) &&
          !VM_EVAL_COMPLETED_ALL(thread));

  if(VM_IS_SET(expr->flags, VM_EXPR_LAMBDA) && !VM_EVAL_COMPLETED(thread, 0)) {
    VM_EVAL_SET_REQUESTED(thread, 0);
    return;
  }

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  vm_print_eval_expr(thread);
#endif

  if(VM_IS_SET(expr->flags, VM_EXPR_RESTART)) {
    return;
  } else if(thread->exprc > 1) {
    /* Replace the evaluated object with the result of the evaluation. */
    vm_thread_stack_pop(thread);
    expr = thread->expr;

    /* Snapshot argv[0] before overwrite: if arg 0 was a sub-expression in
       operator position (VM_FORM_REF), the result is the operator value
       and must trigger lambda execution. If it was already a lambda or
       closure being invoked (loaded inline as VM_FORM_LAMBDA or stored
       in argv[0] as a VM_TYPE_CLOSURE before dispatch), the result is the
       body's return value and must not re-invoke. */
    int prev_arg0_was_subexpr =
      (expr->eval_arg == 0 &&
       expr->argv[0].type == VM_TYPE_FORM &&
       expr->argv[0].value.form.type == VM_FORM_REF);

    VM_EVAL_SET_COMPLETED(thread, expr->eval_arg);
    memmove(&expr->argv[expr->eval_arg], &thread->result, sizeof(vm_obj_t));

    /* When evaluating a sub-expression in operator position produced a
       lambda or closure (e.g. ((box-ref f) 0) or ((make-fn) x)), set up
       lambda execution here -- the first-load hook around line 230 only
       handles inline-loaded operators. The completed bit on arg 0 is
       cleared so the eval loop dispatches into the lambda body just as
       it would for an inline-loaded lambda. */
    if(prev_arg0_was_subexpr &&
       ((expr->argv[0].type == VM_TYPE_FORM &&
         expr->argv[0].value.form.type == VM_FORM_LAMBDA) ||
        expr->argv[0].type == VM_TYPE_CLOSURE)) {
      init_lambda_execution(thread, expr);
      expr->eval_completed &= ~(1U << 0);
    }
  } else if(expr->ip == expr->end) {
    /* We have executed the last instruction of the top-level expr.
       For an ordinary thread this means it is finished and will be
       destroyed; for the REPL main thread we park instead so the
       next REPL turn can redirect it to the next entry expression. */
#ifdef VM_REPL_ENABLE
    if(thread->repl_main) {
      thread->status = VM_THREAD_PARKED;
    } else {
      thread->status = VM_THREAD_FINISHED;
    }
#else
    thread->status = VM_THREAD_FINISHED;
#endif
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
#ifdef VM_REPL_ENABLE
    case VM_THREAD_PARKED:
      /* Wait for the next vm_repl_run to redirect this thread to a
         new entry expression. Don't promote VM_RESULT_FINISHED to
         VM_RESULT_RUNNING/SLEEPING -- the parked thread isn't doing
         work, so the caller can return from vm_run as usual. */
      break;
#endif
    default:
      break;
    }

    current_thread = NULL;
  }

  return result;
}
