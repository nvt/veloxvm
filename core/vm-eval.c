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

#include "vm.h"
#include "vm-log.h"
#include "vm-profiler.h"

void
vm_eval_object(vm_thread_t *thread, vm_obj_t *obj)
{
  vm_obj_t *resolved_obj;

  /*
   * Symbols are resolved here to their defined values, whereas objects
   * of other types evaluate to themselves. The evaluation is verified
   * to be semantically correct as well; if not, we signal a thread-local
   * error condition.
   */
  switch(obj->type) {
  case VM_TYPE_STRING:
    vm_string_resolve(thread, obj->value.string);
    break;
  case VM_TYPE_SYMBOL:
    resolved_obj = vm_symbol_resolve(thread, &obj->value.symbol_ref);
    if(resolved_obj == NULL) {
      vm_signal_error(thread, VM_ERROR_SYMBOL_ID);
    } else if(resolved_obj->type == VM_TYPE_NONE) {
      vm_signal_error(thread, VM_ERROR_SYMBOL_UNDEFINED);
      vm_set_error_object(thread, obj);
    } else {
      memmove(obj, resolved_obj, sizeof(vm_obj_t));
    }
    break;
  case VM_TYPE_RATIONAL:
    if(obj->value.rational->denominator == 0) {
      vm_signal_error(thread, VM_ERROR_DIV0);
    }
  default:
    break;
  }
}

void
vm_eval_expr(vm_thread_t *thread, vm_expr_t *expr)
{
  vm_obj_t *op;
  const vm_procedure_t *proc;
  vm_type_set_t type_set;
  int i;
#if VM_PROFILER_ENABLE
  vm_symbol_id_t op_symbol_id;
#endif

  if(expr->argc == 0) {
    /* This is a special case indicating that we got
       an atom instead of a form. */
    vm_eval_object(thread, &expr->argv[0]);
    memcpy(&thread->result, &expr->argv[0], sizeof(vm_obj_t));
    return;
  }

  op = expr->argv;
#if VM_PROFILER_ENABLE
  op_symbol_id = ~(vm_symbol_id_t)0;
#endif

  switch(op->type) {
  case VM_TYPE_SYMBOL:
    proc = vm_procedure_lookup(thread->program, &op->value.symbol_ref);
#if VM_PROFILER_ENABLE
    op_symbol_id = op->value.symbol_ref.symbol_id;
#endif
    break;
  case VM_TYPE_PROCEDURE:
    proc = op->value.procedure;
    break;
  case VM_TYPE_FORM:
    VM_EVAL_ARG(thread, 0);
    return;
  default:
    return;
  }

  if(proc == NULL || proc->operator == NULL) {
    vm_signal_error(thread, VM_ERROR_BYTECODE);
    return;
  }

  if(expr->argc - 1 < proc->min_args ||
     (proc->max_args >= 0 && expr->argc - 1 > proc->max_args)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  if(proc->flags == VM_PROCEDURE_EVAL_ARGS &&
     expr->argc > 1 &&
     expr->eval_requested == 1 &&
     expr->eval_completed == 1) {
    expr->eval_requested = (1 << expr->argc) - 1;
    return;
  }

  type_set = 0;
  for(i = 1; i < expr->argc; i++) {
    SET(type_set, VM_TYPE_FLAG(expr->argv[i].type));
  }

  if((type_set & proc->valid_types) == type_set) {
    /* Set the default result type to VM_TYPE_NONE, which means that the
       operator did not return any object. */
    thread->result.type = VM_TYPE_NONE;
    expr->procedure = proc;

    VM_PROFILER_INVOCATION_START(op_symbol_id);

    /* Execute a native function. */
    proc->operator(thread, type_set, expr->argc - 1, expr->argv + 1);

    VM_PROFILER_INVOCATION_END(op_symbol_id);
    thread->stats.function_calls++;
#if VM_INSTRUCTION_PROFILING
    thread->program->exec_count[expr->expr_id]++;
#endif
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    vm_set_error_object(thread, &expr->argv[0]);
  }
}

void
vm_print_eval_expr(vm_thread_t *thread)
{
  vm_expr_t *expr;
  int i;

  if(VM_DEBUG_LEVEL <= VM_DEBUG_MEDIUM && thread->exprc > 1) {
    return;
  }
  expr = thread->expr;

  VM_PRINTF("[%s:%u] EVAL ", thread->program->name, (unsigned)thread->id);
  if(expr->argc == 0) {
    vm_write_object(NULL, &expr->argv[0]);
  } else {
    VM_PRINTF("(");

    for(i = 0; i < expr->argc; i++) {
      if(i > 0) {
	VM_PRINTF(" ");
      }
      vm_write_object(NULL, &expr->argv[i]);
    }
    VM_PRINTF(")");
    if(thread->result.type != VM_TYPE_NONE) {
      VM_PRINTF(" => ");
      vm_write_object(NULL, &thread->result);
    }
  }

  VM_PRINTF("\n");
}

static void
print_frame(vm_thread_t *thread, unsigned depth)
{
  vm_expr_t *expr;
  unsigned i;

  if(depth == 0) {
    VM_PRINTF("(<Main frame>)\n");
    return;
  } else if(depth >= thread->exprc) {
    VM_PRINTF("<Invalid frame>\n");
    return;
  }


  expr = thread->exprv[depth];

  if(expr->argc == 0) {
    VM_PRINTF("Object ", (unsigned)thread->id);
    vm_write_object(NULL, &expr->argv[0]);
    VM_PRINTF("\n");
  } else {
    VM_PRINTF("(");
    for(i = 0; i < expr->argc; i++) {
      if(i > 0) {
	VM_PRINTF(" ");
      }
      if(i == expr->eval_arg) {
	VM_PRINTF("*");
      }
      vm_write_object(NULL, &expr->argv[i]);
      if(i == expr->eval_arg) {
	VM_PRINTF("*");
      }
    }
    VM_PRINTF(") ; Form %u\n", expr->expr_id);
  }
}

void
vm_print_stack_trace(vm_thread_t *thread)
{
  int i;
  int j;

  VM_PRINTF("-- Stack trace for thread %u ---\n", (unsigned)thread->id);
  for(i = thread->exprc - 1; i >= 0; i--) {
    VM_PRINTF("Frame %02d:", i);
    for(j = 0; j < thread->exprc - i - 1; j++) {
      VM_PRINTF(" ");
    }
    print_frame(thread, i);
  }
  VM_PRINTF("--- End of stack trace ---\n");
}

void
vm_print_eval_object(vm_thread_t *thread, vm_obj_t *obj)
{
  VM_PRINTF("[%u] EVAL: ", (unsigned)thread->id);
  vm_write_object(NULL, obj);
  VM_PRINTF("\n");
}
