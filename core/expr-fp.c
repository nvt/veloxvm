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
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
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

/*
 * expr-fp: This module implements higher-order functions for list
 *          processing. It does so by creating a synthetic stack
 *          frame, which contains the lambda expression to apply to
 *          each element in the list being processed. Since this stack
 *          frame must be kept as long as the higher-order function is
 *          running, the stack pop function will not deallocate it as
 *          it normally would have done.
 *
 *          The result that is being generated as the higher-order
 *          function executes will be stored as an extra "invisible"
 *          argument in the higher-order function's own stack
 *          frame. Once the higher-order function has completed, the
 *          result is is copied from the argument object into the
 *          thread's result object, and the synthetic stack frame is
 *          deallocated.
 */

#include "vm-functions.h"
#include "vm-log.h"
#include "vm-pair.h"

static int
needs_further_eval(vm_thread_t *thread, vm_integer_t argc, vm_obj_t *argv)
{
  if(!VM_EVAL_ARG_DONE(thread, 0) &&
     argv[0].type == VM_TYPE_FORM &&
     argv[0].value.form.type != VM_FORM_LAMBDA) {
    VM_EVAL_ARG(thread, 0);
    return 1;
  }

  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 1);
    return 1;
  }

  return 0;
}

static void
execute_synthetic_expr(vm_thread_t *thread, vm_expr_t *expr,
		       vm_obj_t *function, int result_dest)
{
  if(thread->exprc >= VM_CONTEXT_STACK_SIZE) {
    vm_signal_error(thread, VM_ERROR_STACK_OVERFLOW);
    return;
  }

  /* Store the result of the synthetic expression in argument "result_dest"
     of the current expression. */
  thread->expr->eval_arg = result_dest;

  thread->exprv[thread->exprc++] = expr;
  thread->expr = expr;

  /* Ensure that the expression is not de-allocated through a
     frame pop operation. */
  VM_SET_FLAG(expr->flags, VM_EXPR_SAVE_FRAME);

  /* Restore the expression, which may have been overwritten during
     the execution of a lambda function. */
  memcpy(&expr->argv[0], function, sizeof(vm_obj_t));

  /* Force the synthetic expression to be evaluated. */
  expr->eval_requested = 1;
  expr->eval_completed = 0;
}

/* Advance argv[1] one step through the input list/pair, copying the
   current car into *car_out. Returns 1 if a car was yielded, 0 if
   end-of-input, -1 on improper termination or type error. */
static int
fp_step_input(vm_obj_t *input_slot, vm_obj_t *car_out)
{
  if(input_slot->type == VM_TYPE_NIL) {
    return 0;
  }
  if(input_slot->type == VM_TYPE_PAIR) {
    if(input_slot->value.pair == NULL) {
      return 0;
    }
    *car_out = input_slot->value.pair->car;
    *input_slot = input_slot->value.pair->cdr;
    return 1;
  }
  return -1;
}

/* Append a fresh pair (car = obj, cdr = nil) onto the chain whose
   head is *head_slot and tail is *tail_slot. Both slots may start as
   VM_TYPE_NIL (empty chain). Returns 0 on heap exhaustion. */
static int
fp_append_pair(vm_obj_t *head_slot, vm_obj_t *tail_slot,
               const vm_obj_t *obj)
{
  vm_pair_t *p = vm_alloc(sizeof(vm_pair_t));
  if(p == NULL) {
    return 0;
  }
  p->car = *obj;
  p->cdr.type = VM_TYPE_NIL;
  if(tail_slot->type == VM_TYPE_NIL) {
    head_slot->type = VM_TYPE_PAIR;
    head_slot->value.pair = p;
  } else {
    tail_slot->value.pair->cdr.type = VM_TYPE_PAIR;
    tail_slot->value.pair->cdr.value.pair = p;
  }
  tail_slot->type = VM_TYPE_PAIR;
  tail_slot->value.pair = p;
  return 1;
}

VM_FUNCTION(map)
{
  vm_expr_t *current_expr;
  vm_expr_t *map_expr;
  vm_obj_t next_car;
  int step;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(argv[1].type != VM_TYPE_PAIR && argv[1].type != VM_TYPE_NIL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;

  /* Iterative-scheduler state stored in argv:
       argv[1]            input cursor (advances each iteration)
       argv[argc-3]       result head pair (or NIL while empty)
       argv[argc-2]       result tail pair (or NIL while empty)
       argv[argc-1]       intermediate result of the procedure call */
  if(argc == 2) {
    /* Initiate the MAP operation. */
    map_expr = vm_thread_stack_alloc(thread);
    if(map_expr == NULL) {
      return;
    }
    map_expr->flags = VM_EXPR_HAVE_OBJECTS;
    map_expr->argc = 2;

    vm_gc_disable();
    current_expr->argc += 3;
    current_expr->argv[current_expr->argc - 3].type = VM_TYPE_NIL;
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_NIL;
    vm_gc_enable();
  } else if(argc >= 5) {
    map_expr = thread->exprv[thread->exprc];
    if(map_expr == NULL) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* Append the procedure result to the head/tail-tracked chain. */
    if(!fp_append_pair(&argv[argc - 3], &argv[argc - 2],
                       &argv[argc - 1])) {
      vm_thread_stack_free(map_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  /* Try to advance the input cursor; on end, return the result head.
     Head lives at argv[2] (stable index, set during init). At STOP
     time current_expr->argc may have been grown by the scheduler
     beyond its init value, so we can't use argc-relative addressing
     here. */
  step = fp_step_input(&argv[1], &next_car);
  if(step <= 0) {
    thread->result = argv[2];
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(map_expr);
    if(step < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    }
    return;
  }

  memcpy(&map_expr->argv[1], &next_car, sizeof(vm_obj_t));

  /* Set the current expression to be the synthetic MAP expression. */
  execute_synthetic_expr(thread, map_expr, &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(filter)
{
  vm_expr_t *current_expr;
  vm_expr_t *filter_expr;
  vm_obj_t next_car;
  int step;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(argv[1].type != VM_TYPE_PAIR && argv[1].type != VM_TYPE_NIL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;

  /* Same iterative-scheduler state layout as map (see comment there).
     argv[argc-1] is the predicate's truth result, not a mapped value;
     when truthy, we append the original element (saved in
     filter_expr->argv[1]) to the result chain. */
  if(argc == 2) {
    filter_expr = vm_thread_stack_alloc(thread);
    if(filter_expr == NULL) {
      return;
    }
    filter_expr->flags = VM_EXPR_HAVE_OBJECTS;
    filter_expr->argc = 2;

    vm_gc_disable();
    current_expr->argc += 3;
    current_expr->argv[current_expr->argc - 3].type = VM_TYPE_NIL;
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_NIL;
    vm_gc_enable();
  } else if(argc >= 5) {
    filter_expr = thread->exprv[thread->exprc];

    if(argv[argc - 1].value.boolean == VM_TRUE) {
      if(!fp_append_pair(&argv[argc - 3], &argv[argc - 2],
                         &filter_expr->argv[1])) {
        vm_thread_stack_free(filter_expr);
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  filter_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  step = fp_step_input(&argv[1], &next_car);
  if(step <= 0) {
    thread->result = argv[2];
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(filter_expr);
    if(step < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    }
    return;
  }

  memcpy(&filter_expr->argv[1], &next_car, sizeof(vm_obj_t));

  /* Set the current expression to be the synthetic FILTER expression. */
  execute_synthetic_expr(thread, filter_expr,
			 &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(for_each)
{
  vm_expr_t *current_expr;
  vm_expr_t *foreach_expr;
  vm_obj_t next_car;
  int step;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(!vm_is_procedure(thread, &argv[0]) ||
     (argv[1].type != VM_TYPE_PAIR && argv[1].type != VM_TYPE_NIL)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;

  if(argc == 2) {
    if(argv[1].type == VM_TYPE_NIL) {
      /* Avoid processing empty lists further. */
      VM_EVAL_STOP(thread);
      return;
    }

    /* Initiate the FOR-EACH operation. */
    foreach_expr = vm_thread_stack_alloc(thread);
    if(foreach_expr == NULL) {
      return;
    }
    foreach_expr->flags = VM_EXPR_HAVE_OBJECTS;
    foreach_expr->argc = 2;

    /* Create a new argument that stores the discarded synthetic
       call result. */
    current_expr->argc++;
  } else {
    foreach_expr = thread->exprv[thread->exprc];
  }

  foreach_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  step = fp_step_input(&argv[1], &next_car);
  if(step <= 0) {
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(foreach_expr);
    if(step < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    }
    return;
  }
  memcpy(&foreach_expr->argv[1], &next_car, sizeof(vm_obj_t));

  execute_synthetic_expr(thread, foreach_expr, &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(reduce)
{
  vm_expr_t *current_expr;
  vm_expr_t *reduce_expr;
  vm_obj_t next_car;
  int step;

  if(needs_further_eval(thread, argc, argv)) {
    memset(&argv[argc], 0, sizeof(vm_obj_t));
    return;
  }

  if(argv[1].type != VM_TYPE_PAIR && argv[1].type != VM_TYPE_NIL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;

  if(argc == 2) {
    /* Initiate the 2-arg REDUCE: first element becomes the
       accumulator, rest are reduced into it. */
    reduce_expr = vm_thread_stack_alloc(thread);
    if(reduce_expr == NULL) {
      return;
    }
    reduce_expr->flags = VM_EXPR_HAVE_OBJECTS;
    reduce_expr->argc = 3;

    memcpy(&reduce_expr->argv[0], &argv[0], sizeof(vm_obj_t));

    /* Add accumulator slot. */
    current_expr->argc++;

    /* Consume the first element as accumulator and advance input. */
    step = fp_step_input(&argv[1], &next_car);
    if(step <= 0) {
      current_expr->argv[current_expr->argc - 1].type = VM_TYPE_NONE;
    } else {
      memcpy(&reduce_expr->argv[1], &next_car, sizeof(vm_obj_t));
      memcpy(&current_expr->argv[current_expr->argc - 1], &next_car,
             sizeof(vm_obj_t));
    }
    current_expr->eval_arg = current_expr->argc - 1;
  } else if(argc == 3) {
    reduce_expr = thread->exprv[thread->exprc];

    if(reduce_expr == NULL || !VM_IS_SET(reduce_expr->flags, VM_EXPR_SAVE_FRAME)) {
      /* Initiate the 3-arg REDUCE: explicit initial accumulator. */
      reduce_expr = vm_thread_stack_alloc(thread);
      if(reduce_expr == NULL) {
        return;
      }
      reduce_expr->flags = VM_EXPR_HAVE_OBJECTS;
      reduce_expr->argc = 3;

      memcpy(&reduce_expr->argv[0], &argv[0], sizeof(vm_obj_t));

      current_expr->argc++;
      memcpy(&reduce_expr->argv[1], &argv[2], sizeof(vm_obj_t));
      memcpy(&current_expr->argv[current_expr->argc - 1], &argv[2],
             sizeof(vm_obj_t));
      current_expr->eval_arg = current_expr->argc - 1;
    } else {
      /* Continue the 2-arg REDUCE: synthetic call returned the new
         accumulator at argv[2]. */
      memcpy(&reduce_expr->argv[1], &argv[2], sizeof(vm_obj_t));
    }
  } else if(argc == 4) {
    /* Continue the 3-arg REDUCE. */
    reduce_expr = thread->exprv[thread->exprc];
    if(reduce_expr == NULL) {
      vm_signal_error(thread, VM_ERROR_INTERNAL);
      return;
    }
    memcpy(&reduce_expr->argv[1], &argv[3], sizeof(vm_obj_t));
  } else {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
    return;
  }

  reduce_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  /* Try to pull the next element to feed into the reducer. End of
     input -> push the accumulator and stop. */
  step = fp_step_input(&argv[1], &next_car);
  if(step <= 0) {
    VM_EVAL_STOP(thread);
    VM_PUSH(&current_expr->argv[current_expr->argc - 1]);
    vm_thread_stack_free(reduce_expr);
    return;
  }
  memcpy(&reduce_expr->argv[2], &next_car, sizeof(vm_obj_t));

  execute_synthetic_expr(thread, reduce_expr, &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(count)
{
  vm_expr_t *current_expr;
  vm_expr_t *count_expr;
  vm_obj_t next_car;
  int step;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(argv[1].type != VM_TYPE_PAIR && argv[1].type != VM_TYPE_NIL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;

  if(argc == 2) {
    /* Initiate the COUNT operation. Stable slots:
         argv[2] = running counter (INTEGER)
         argv[argc-1] = synthetic predicate result (BOOLEAN, set
                       by scheduler when synthetic returns). */
    count_expr = vm_thread_stack_alloc(thread);
    if(count_expr == NULL) {
      return;
    }
    count_expr->flags = VM_EXPR_HAVE_OBJECTS;
    count_expr->argc = 2;

    /* Append the running counter as a new stable slot. The frame holds
       [operator, predicate, list]; the counter must go in the slot added
       by the argc bump (frame argv[3], i.e. this function's argv[2], which
       is where the re-entry path below reads it), NOT argv[2], which is the
       list argument and would be clobbered. */
    current_expr->argc += 2;
    current_expr->argv[3].type = VM_TYPE_INTEGER;
    current_expr->argv[3].value.integer = 0;
  } else if(argc >= 4) {
    count_expr = thread->exprv[thread->exprc];

    if(argv[2].type != VM_TYPE_INTEGER ||
       argv[argc - 1].type != VM_TYPE_BOOLEAN) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* The predicate holds for the last evaluated object,
       so we increase the count. */
    if(argv[argc - 1].value.boolean == VM_TRUE) {
      argv[2].value.integer++;
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  count_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  step = fp_step_input(&argv[1], &next_car);
  if(step <= 0) {
    VM_PUSH(&argv[2]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(count_expr);
    if(step < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    }
    return;
  }
  memcpy(&count_expr->argv[1], &next_car, sizeof(vm_obj_t));

  /* Set the current expression to be the synthetic COUNT expression. */
  execute_synthetic_expr(thread, count_expr, &argv[0], current_expr->argc - 1);
}

/*
 * Vector higher-order operators. These mirror the list versions above but
 * iterate the input by index rather than by walking a chain of cons cells,
 * so they avoid materialising an intermediate list. The state per
 * invocation lives in extra slots appended to current_expr->argv:
 *   argv[argc-3] (vector_count, vector_map): integer index
 *   argv[argc-2]                            : accumulator (count, list, ...)
 *   argv[argc-1]                            : predicate result slot
 * vector-for-each uses two extras (index + result slot); vector-fold uses
 * two (index + accumulator) because the explicit init occupies argv[1].
 */

static void
vector_read_element(vm_vector_t *vector, vm_integer_t index,
                    vm_obj_t *out)
{
  if(VM_IS_SET(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    out->type = VM_TYPE_CHARACTER;
    out->value.character = vector->bytes[index];
  } else {
    memcpy(out, &vector->elements[index], sizeof(vm_obj_t));
  }
}

VM_FUNCTION(vector_for_each)
{
  vm_vector_t *vector;
  vm_expr_t *current_expr;
  vm_expr_t *foreach_expr;
  vm_integer_t index;
  vm_obj_t element;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(!vm_is_procedure(thread, &argv[0]) ||
     argv[1].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;
  vector = argv[1].value.vector;

  if(argc == 2) {
    /* Initial entry: allocate state, reserve slots for index + result. */
    foreach_expr = vm_thread_stack_alloc(thread);
    if(foreach_expr == NULL) {
      return;
    }
    foreach_expr->flags = VM_EXPR_HAVE_OBJECTS;
    foreach_expr->argc = 2;

    current_expr->argc += 2;
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_INTEGER;
    current_expr->argv[current_expr->argc - 2].value.integer = 0;
  } else if(argc >= 4) {
    /* Resumption: discard the predicate result, advance the index. */
    foreach_expr = thread->exprv[thread->exprc];
    current_expr->argv[current_expr->argc - 2].value.integer++;
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  foreach_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  index = current_expr->argv[current_expr->argc - 2].value.integer;
  if(index >= vector->length) {
    /* Done. for-each has no return value. */
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(foreach_expr);
    return;
  }

  vector_read_element(vector, index, &element);
  memcpy(&foreach_expr->argv[1], &element, sizeof(vm_obj_t));

  execute_synthetic_expr(thread, foreach_expr, &argv[0],
                         current_expr->argc - 1);
}

VM_FUNCTION(vector_count)
{
  vm_vector_t *vector;
  vm_expr_t *current_expr;
  vm_expr_t *count_expr;
  vm_integer_t index;
  vm_obj_t element;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(!vm_is_procedure(thread, &argv[0]) ||
     argv[1].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;
  vector = argv[1].value.vector;

  if(argc == 2) {
    /* Initial entry: allocate state, reserve slots for
       index, count, predicate result. */
    count_expr = vm_thread_stack_alloc(thread);
    if(count_expr == NULL) {
      return;
    }
    count_expr->flags = VM_EXPR_HAVE_OBJECTS;
    count_expr->argc = 2;

    current_expr->argc += 3;
    current_expr->argv[current_expr->argc - 3].type = VM_TYPE_INTEGER;
    current_expr->argv[current_expr->argc - 3].value.integer = 0;
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_INTEGER;
    current_expr->argv[current_expr->argc - 2].value.integer = 0;
  } else if(argc >= 5) {
    count_expr = thread->exprv[thread->exprc];

    if(argv[argc - 1].type != VM_TYPE_BOOLEAN) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    if(argv[argc - 1].value.boolean == VM_TRUE) {
      current_expr->argv[current_expr->argc - 2].value.integer++;
    }
    current_expr->argv[current_expr->argc - 3].value.integer++;
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  count_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  index = current_expr->argv[current_expr->argc - 3].value.integer;
  if(index >= vector->length) {
    VM_PUSH(&current_expr->argv[current_expr->argc - 2]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(count_expr);
    return;
  }

  vector_read_element(vector, index, &element);
  memcpy(&count_expr->argv[1], &element, sizeof(vm_obj_t));

  execute_synthetic_expr(thread, count_expr, &argv[0],
                         current_expr->argc - 1);
}

VM_FUNCTION(vector_fold)
{
  vm_vector_t *vector;
  vm_expr_t *current_expr;
  vm_expr_t *fold_expr;
  vm_integer_t index;
  vm_obj_t element;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }
  /* needs_further_eval covers args 0 and 1; vector-fold takes a third
     argument (the vector) that must also be evaluated before use. */
  if(argc >= 3 && !VM_EVAL_ARG_DONE(thread, 2)) {
    VM_EVAL_ARG(thread, 2);
    return;
  }

  if(!vm_is_procedure(thread, &argv[0]) ||
     argv[2].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;
  vector = argv[2].value.vector;

  if(argc == 3) {
    /* Initial entry: synthetic call shape is (f acc element), so the
       synthetic frame holds three slots (function + acc + element).
       Reserve two slots in the parent frame for the running index and
       the accumulator (which is also the result destination). */
    fold_expr = vm_thread_stack_alloc(thread);
    if(fold_expr == NULL) {
      return;
    }
    fold_expr->flags = VM_EXPR_HAVE_OBJECTS;
    fold_expr->argc = 3;

    current_expr->argc += 2;
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_INTEGER;
    current_expr->argv[current_expr->argc - 2].value.integer = 0;
    /* Seed the accumulator from the caller's init argument. */
    memcpy(&current_expr->argv[current_expr->argc - 1], &argv[1],
           sizeof(vm_obj_t));
  } else if(argc >= 5) {
    fold_expr = thread->exprv[thread->exprc];
    /* The predicate's return value is the new accumulator and already
       lives in argv[argc-1] (the eval result destination). Just advance
       the index. */
    current_expr->argv[current_expr->argc - 2].value.integer++;
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  fold_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  index = current_expr->argv[current_expr->argc - 2].value.integer;
  if(index >= vector->length) {
    VM_PUSH(&current_expr->argv[current_expr->argc - 1]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(fold_expr);
    return;
  }

  vector_read_element(vector, index, &element);
  /* Synthetic call: (f acc element). */
  memcpy(&fold_expr->argv[1],
         &current_expr->argv[current_expr->argc - 1], sizeof(vm_obj_t));
  memcpy(&fold_expr->argv[2], &element, sizeof(vm_obj_t));

  execute_synthetic_expr(thread, fold_expr, &argv[0],
                         current_expr->argc - 1);
}

VM_FUNCTION(vector_map)
{
  vm_vector_t *vector;
  vm_vector_t *result_vector;
  vm_expr_t *current_expr;
  vm_expr_t *map_expr;
  vm_integer_t index;
  vm_obj_t element;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(!vm_is_procedure(thread, &argv[0]) ||
     argv[1].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;
  vector = argv[1].value.vector;

  if(argc == 2) {
    /* Initial entry: pre-allocate the result vector at full length, then
       fill element-by-element as the predicate returns. */
    map_expr = vm_thread_stack_alloc(thread);
    if(map_expr == NULL) {
      return;
    }
    map_expr->flags = VM_EXPR_HAVE_OBJECTS;
    map_expr->argc = 2;

    /* Disable GC while the result vector is allocated and the new argv
       slots are being initialised, to keep the half-built state from
       being marked as live garbage. */
    vm_gc_disable();

    current_expr->argc += 3;

    result_vector = vm_vector_create(
      &current_expr->argv[current_expr->argc - 2],
      vector->length, VM_VECTOR_FLAG_REGULAR);
    if(result_vector == NULL) {
      vm_gc_enable();
      vm_thread_stack_free(map_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    /* Initialise elements to a known type so a mid-iteration GC pass
       walking the result vector finds well-formed entries. */
    for(index = 0; index < result_vector->length; index++) {
      result_vector->elements[index].type = VM_TYPE_NONE;
    }

    current_expr->argv[current_expr->argc - 3].type = VM_TYPE_INTEGER;
    current_expr->argv[current_expr->argc - 3].value.integer = 0;

    vm_gc_enable();
  } else if(argc >= 5) {
    map_expr = thread->exprv[thread->exprc];

    if(current_expr->argv[current_expr->argc - 2].type != VM_TYPE_VECTOR) {
      vm_thread_stack_free(map_expr);
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* Store the predicate's return value into the next slot of the
       result vector, then advance the index. */
    result_vector = current_expr->argv[current_expr->argc - 2].value.vector;
    index = current_expr->argv[current_expr->argc - 3].value.integer;
    memcpy(&result_vector->elements[index],
           &argv[argc - 1], sizeof(vm_obj_t));
    current_expr->argv[current_expr->argc - 3].value.integer++;
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  map_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  index = current_expr->argv[current_expr->argc - 3].value.integer;
  if(index >= vector->length) {
    VM_PUSH(&current_expr->argv[current_expr->argc - 2]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(map_expr);
    return;
  }

  vector_read_element(vector, index, &element);
  memcpy(&map_expr->argv[1], &element, sizeof(vm_obj_t));

  execute_synthetic_expr(thread, map_expr, &argv[0],
                         current_expr->argc - 1);
}
