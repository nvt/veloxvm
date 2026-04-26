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
#include "vm-list.h"

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

VM_FUNCTION(map)
{
  vm_list_t *list;
  vm_list_t *result_list;
  vm_expr_t *current_expr;
  vm_expr_t *map_expr;
  vm_obj_t *obj;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;
  current_expr = thread->expr;

  if(argc == 2) {
    /* Initiate the MAP operation. */
    map_expr = vm_thread_stack_alloc(thread);
    if(map_expr == NULL) {
      return;
    }
    map_expr->flags = VM_EXPR_HAVE_OBJECTS;
    map_expr->argc = 2;

    /* Disable GC while creating list and updating argc/argv to prevent
       GC from marking uninitialized argv slots */
    vm_gc_disable();

    /* Create two new arguments that store the resulting list and the
       intermediate result of the mapping on each element. */
    current_expr->argc += 2;

    result_list = vm_list_create();
    if(result_list == NULL) {
      vm_gc_enable();
      vm_thread_stack_free(map_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_LIST;
    current_expr->argv[current_expr->argc - 2].value.list = result_list;

    vm_gc_enable();
  } else if(argc >= 4) {
    map_expr = thread->exprv[thread->exprc];
    if(map_expr == NULL) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    if(argv[argc - 2].type != VM_TYPE_LIST) {
      vm_thread_stack_free(map_expr);
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* Insert the mapped object into the result list. */
    if(vm_list_insert_tail(argv[argc - 2].value.list,
			   &argv[argc - 1]) == VM_FALSE) {
      vm_list_destroy(argv[argc - 2].value.list);
      vm_thread_stack_free(map_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  if(list->length == 0) {
    /* The list has been processed. Stop the evaluation. */
    VM_PUSH(&current_expr->argv[current_expr->argc - 2]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(map_expr);
    return;
  }

  /* Process the next element in the list upon the next invocation of MAP. */
  obj = vm_list_car(list);
  if(obj != NULL) {
    memcpy(&map_expr->argv[1], obj, sizeof(vm_obj_t));
    argv[1].value.list = vm_list_cdr(list, 1);
  } else {
    map_expr->argc--;
  }

  /* Set the current expression to be the synthetic MAP expression. */
  execute_synthetic_expr(thread, map_expr, &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(filter)
{
  vm_list_t *list;
  vm_list_t *result_list;
  vm_expr_t *current_expr;
  vm_expr_t *filter_expr;
  vm_obj_t *obj;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;
  current_expr = thread->expr;

  if(argc == 2) {
    /* Initiate the FILTER operation. */
    filter_expr = vm_thread_stack_alloc(thread);
    if(filter_expr == NULL) {
      return;
    }
    filter_expr->flags = VM_EXPR_HAVE_OBJECTS;
    filter_expr->argc = 2;

    /* Disable GC while creating list and updating argc/argv to prevent
       GC from marking uninitialized argv slots */
    vm_gc_disable();

    /* Create two new arguments that store the resulting list and the
       intermediate result of the mapping on each element. */
    current_expr->argc += 2;

    result_list = vm_list_create();
    if(result_list == NULL) {
      vm_gc_enable();
      vm_thread_stack_free(filter_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    current_expr->argv[current_expr->argc - 2].type = VM_TYPE_LIST;
    current_expr->argv[current_expr->argc - 2].value.list = result_list;

    vm_gc_enable();
  } else if(argc >= 4) {
    filter_expr = thread->exprv[thread->exprc];
    if(argv[argc - 2].type != VM_TYPE_LIST) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* Insert the filtered object into the result list. */
    if(argv[argc - 1].value.boolean == VM_TRUE &&
       vm_list_insert_tail(argv[argc - 2].value.list,
			   &filter_expr->argv[1]) == VM_FALSE) {
      vm_list_destroy(argv[argc - 2].value.list);
      vm_thread_stack_free(filter_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  filter_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  if(list->length == 0) {
    /* The list has been processed. Stop the evaluation.
     * Use current_expr->argc (post-init) so that filtering an empty
     * list returns the accumulator rather than the predicate; the
     * caller's argc parameter is still the pre-init value. */
    VM_PUSH(&current_expr->argv[current_expr->argc - 2]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(filter_expr);
    return;
  }

  /* Process the next element in the list upon the next invocation of FILTER. */
  obj = vm_list_car(list);
  if(obj != NULL) {
    memcpy(&filter_expr->argv[1], obj, sizeof(vm_obj_t));
    argv[1].value.list = vm_list_cdr(list, 1);
  } else {
    filter_expr->argc--;
  }

  /* Set the current expression to be the synthetic FILTER expression. */
  execute_synthetic_expr(thread, filter_expr,
			 &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(for_each)
{
  vm_list_t *list;
  vm_expr_t *current_expr;
  vm_expr_t *foreach_expr;
  vm_obj_t *obj;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(!vm_is_procedure(thread, &argv[0]) || argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  current_expr = thread->expr;
  list = argv[1].value.list;

  if(argc == 2) {
    if(list->length == 0) {
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

    /* Create a new argument that stores the object
       currently being processed. */
    current_expr->argc++;
  } else {
    foreach_expr = thread->exprv[thread->exprc];
  }

  foreach_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  /* Process the next element in the list upon
     the next invocation of FOR-EACH. */
  obj = vm_list_car(list);
  if(obj == NULL) {
    /* The list has been processed. Stop the evaluation and
       deallocate the synthetic expression. */
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(foreach_expr);
    return;
  }
  memcpy(&foreach_expr->argv[1], obj, sizeof(vm_obj_t));

  /* Remove the processed object from the list. */
  argv[1].value.list = vm_list_cdr(list, 1);
  if(argv[1].value.list == NULL) {
    vm_thread_stack_free(foreach_expr);
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  execute_synthetic_expr(thread, foreach_expr, &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(reduce)
{
  vm_list_t *list;
  vm_expr_t *current_expr;
  vm_expr_t *reduce_expr;
  vm_obj_t *obj;
  int skip_first_element;

  if(needs_further_eval(thread, argc, argv)) {
    memset(&argv[argc], 0, sizeof(vm_obj_t));
    return;
  }

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;
  current_expr = thread->expr;
  skip_first_element = 0;

  if(argc == 2) {
    /* Initiate the REDUCE operation without initial value. */
    reduce_expr = vm_thread_stack_alloc(thread);
    if(reduce_expr == NULL) {
      return;
    }
    reduce_expr->flags = VM_EXPR_HAVE_OBJECTS;
    reduce_expr->argc = 3;

    memcpy(&reduce_expr->argv[0], &argv[0], sizeof(vm_obj_t));

    /* Create a new argument that stores the intermediate result of
       the reduction. */
    current_expr->argc++;

    /* Use first element as initial value. */
    obj = vm_list_car(list);
    if(obj == NULL) {
      current_expr->argv[current_expr->argc - 1].type = VM_TYPE_NONE;
    } else {
      memcpy(&reduce_expr->argv[1], obj, sizeof(vm_obj_t));
      /* Also mirror the initial accumulator into the result slot so
       * that a single-element input terminates with the element
       * rather than with the trimmed (empty) tail list. */
      memcpy(&current_expr->argv[current_expr->argc - 1], obj,
             sizeof(vm_obj_t));
    }
    current_expr->eval_arg = current_expr->argc - 1;
    skip_first_element = 1;  /* Skip the element we used as initial value */
  } else if(argc == 3) {
    reduce_expr = thread->exprv[thread->exprc];

    /* Check if this is a continuation (reduce_expr exists and has SAVE_FRAME flag)
       or a new call (reduce_expr is NULL or doesn't have SAVE_FRAME). */
    if(reduce_expr == NULL || !VM_IS_SET(reduce_expr->flags, VM_EXPR_SAVE_FRAME)) {
      /* Initiate the REDUCE operation WITH initial value. */
      reduce_expr = vm_thread_stack_alloc(thread);
      if(reduce_expr == NULL) {
        return;
      }
      reduce_expr->flags = VM_EXPR_HAVE_OBJECTS;
      reduce_expr->argc = 3;

      memcpy(&reduce_expr->argv[0], &argv[0], sizeof(vm_obj_t));

      /* Create a new argument that stores the intermediate result. */
      current_expr->argc++;

      /* Use provided initial value. */
      memcpy(&reduce_expr->argv[1], &argv[2], sizeof(vm_obj_t));
      current_expr->eval_arg = current_expr->argc - 1;
      skip_first_element = 0;  /* Process from first element */
    } else {
      /* Continue the REDUCE operation (2-arg version). */
      memcpy(&reduce_expr->argv[1], &argv[2], sizeof(vm_obj_t));
      skip_first_element = 1;  /* Skip the processed element */
    }
  } else if(argc == 4) {
    /* Continue the REDUCE operation (3-arg version). */
    reduce_expr = thread->exprv[thread->exprc];
    if(reduce_expr == NULL) {
      vm_signal_error(thread, VM_ERROR_INTERNAL);
      return;
    }
    memcpy(&reduce_expr->argv[1], &argv[3], sizeof(vm_obj_t));
    skip_first_element = 1;  /* Skip the processed element */
  } else {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
    return;
  }

  reduce_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  if(list->length == 0) {
    /* We have processed all objects in the list. */
    VM_EVAL_STOP(thread);
    /* Push the accumulated result. Use current_expr->argc, not the
     * caller's argc: on the 2-arg init path argc is still 2 but the
     * accumulator slot has already been appended at argc==3, so the
     * stale value points one slot too low. */
    VM_PUSH(&current_expr->argv[current_expr->argc - 1]);
    vm_thread_stack_free(reduce_expr);
    return;
  }

  /* Process the next element in the list upon the next
     invocation of REDUCE. */
  if(skip_first_element) {
    list = argv[1].value.list = vm_list_cdr(list, 1);
    if(list == NULL) {
      vm_thread_stack_free(reduce_expr);
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  obj = vm_list_car(list);
  if(obj != NULL) {
    memcpy(&reduce_expr->argv[2], obj, sizeof(vm_obj_t));
  } else {
    /* We have processed all objects in the list. */
    VM_EVAL_STOP(thread);
    VM_PUSH(&current_expr->argv[current_expr->argc - 1]);
    vm_thread_stack_free(reduce_expr);
    return;
  }

  execute_synthetic_expr(thread, reduce_expr, &argv[0], current_expr->argc - 1);
}

VM_FUNCTION(count)
{
  vm_list_t *list;
  vm_expr_t *current_expr;
  vm_expr_t *count_expr;
  vm_obj_t *obj;

  if(needs_further_eval(thread, argc, argv)) {
    return;
  }

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;
  current_expr = thread->expr;

  if(argc == 2) {
    /* Initiate the COUNT operation. */
    count_expr = vm_thread_stack_alloc(thread);
    if(count_expr == NULL) {
      return;
    }
    count_expr->flags = VM_EXPR_HAVE_OBJECTS;
    count_expr->argc = 2;

    /*
     * Create two new arguments that store the count variable and the
     * intermediate result of applying the predicate expression on
     * each element.
     */
    current_expr->argc += 2;

    current_expr->argv[3].type = VM_TYPE_INTEGER;
    current_expr->argv[3].value.integer = 0;
  } else if(argc >= 4) {
    count_expr = thread->exprv[thread->exprc];

    if(argv[argc - 2].type != VM_TYPE_INTEGER ||
       argv[argc - 1].type != VM_TYPE_BOOLEAN) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* The predicate holds for the last evaluated object,
       so we increase the count. */
    if(argv[argc - 1].value.boolean == VM_TRUE) {
      argv[argc - 2].value.integer++;
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_COUNT);
    return;
  }

  count_expr->flags &= ~VM_EXPR_SAVE_FRAME;

  if(list->length == 0) {
    /* The list has been processed. Stop the evaluation. */
    VM_PUSH(&argv[argc - 2]);
    VM_EVAL_STOP(thread);
    vm_thread_stack_free(count_expr);
    return;
  }

  /* Process the next element in the list upon the next invocation of COUNT. */
  obj = vm_list_car(list);
  if(obj != NULL) {
    memcpy(&count_expr->argv[1], obj, sizeof(vm_obj_t));
    argv[1].value.list = vm_list_cdr(list, 1);
  } else {
    count_expr->argc--;
  }

  /* Set the current expression to be the synthetic COUNT expression. */
  execute_synthetic_expr(thread, count_expr, &argv[0], 4);
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
