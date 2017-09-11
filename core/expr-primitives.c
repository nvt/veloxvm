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

#include <limits.h>

#include "vm-functions.h"
#include "vm-list.h"
#include "vm-log.h"
#include "vm-native.h"

/* A predicate that shows whether an object is true or false, as
   defined in Scheme R5RS, section 6.3.1. */
static vm_boolean_t
object_is_true(vm_obj_t *obj)
{
  return obj->type != VM_TYPE_BOOLEAN || obj->value.boolean == VM_TRUE;
}

static int
highest_bit_set(vm_integer_t value)
{
  int highest, i;

  if(value == 0) {
    return -1;
  }

  highest = -1;
  for(i = sizeof(value) * CHAR_BIT - 1; i >= 0; i--) {
    if(value & (1 << i)) {
      highest = i;
      break;
    }
  }

  /* Subtract the bit for the operator, since that is not an argument
     presented to a VM_FUNCTION. */
  return highest - 1;
}

VM_FUNCTION(bind)
{
  vm_expr_t *calling_expr;
  int i;

  if(thread->expr->eval_completed == 1) {
    /* Initiate the evaluation of the bind expression. */

    if(argc > 1) {
      /*
       * The bind expression uses all arguments in the calling expression
       * to bind the symbols specified as argument to the bind
       * expression. In addition, the bind operator requires a final
       * argument that is an expression to execute after the symbols have
       * been bound.
       */

      if(thread->exprc < 2) {
	/* Bind cannot execute if there is not a calling expression. */
	vm_signal_error(thread, VM_ERROR_BYTECODE);
	return;
      }

      calling_expr = thread->exprv[thread->exprc - 2];
      if(calling_expr->argc + 1 != thread->expr->argc) {
	/* There must be enough objects in the calling expression to bind
	   the symbols specified as arguments to the called bind
	   expression. */
	vm_signal_error(thread, VM_ERROR_BYTECODE);
	return;
      }

      /* Create a stack for the argument symbol bindings. */
      thread->expr->bindv = VM_MALLOC(sizeof(vm_symbol_bind_t) * (argc - 1));
      if(thread->expr->bindv == NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }

      /* Bind the supplied arguments to the symbols of the bind expression. */
      for(i = 0; i < argc - 1; i++) {
        if(argv[i].type != VM_TYPE_SYMBOL) {
          vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
          return;
        }
        vm_symbol_bind(thread, &argv[i].value.symbol_ref,
                       &calling_expr->argv[i + 1]);
        if(thread->status == VM_THREAD_ERROR) {
          return;
        }
      }
    }

    /* Instruct the scheduler to evaluate the actual lambda expression. */
    VM_EVAL_ARG(thread, argc - 1);
    SET(thread->expr->flags, VM_EXPR_TAIL_CALL);
  } else {
    /* Evaluation finished. */
    VM_EVAL_STOP(thread);
    VM_PUSH(&argv[argc - 1]);
  }
}

VM_FUNCTION(return)
{
  vm_return_from_function(thread, argc > 0 ? &argv[0] : NULL);
}

VM_FUNCTION(begin)
{
  /* Defer the evaluation of the arguments in order to set
     the tail call flag when executing the last argument. */
  VM_EVAL_SET_REQUESTED_RANGE(thread, 1, thread->expr->argc);
  SET(thread->expr->flags, VM_EXPR_TAIL_CALL);
  if(VM_EVAL_COMPLETED(thread, argc - 1)) {
    VM_PUSH(&argv[argc - 1]);
  }
}

VM_FUNCTION(if)
{
  uint8_t use_arg;

  if(!VM_EVAL_ARG_DONE(thread, 0)) {
    VM_EVAL_ARG(thread, 0);
    return;
  }

  use_arg = 2 - (uint8_t)object_is_true(&argv[0]);
  if(use_arg >= argc) {
    VM_EVAL_STOP(thread);
  } else if(!VM_EVAL_ARG_DONE(thread, use_arg)) {
    SET(thread->expr->flags, VM_EXPR_TAIL_CALL);
    VM_EVAL_ARG(thread, use_arg);
  } else {
    VM_PUSH(&argv[use_arg]);
  }
}

VM_FUNCTION(define)
{
  vm_obj_t *obj;
  vm_symbol_bind_t *bindv;

  if(argv[0].type != VM_TYPE_SYMBOL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  } else {
    /* Object definition. */

    if(argv[1].type == VM_TYPE_FORM &&
       argv[1].value.form.type != VM_FORM_LAMBDA &&
       !VM_EVAL_ARG_DONE(thread, 1)) {
      /* Evaluate forms that are not lambda expressions. */
      VM_EVAL_ARG(thread, 1);
      return;
    }

    if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_STRICT_ASSIGNMENT) &&
       thread->exprc > 1) {
      /*
       * Object definitions inside a BIND body are local to that body.
       * thread->exprc must be decreased temporarily in order to put
       * the definition at the level of the BIND expression on the stack.
       */

      if(thread->exprv[thread->exprc - 2]->procedure == NULL ||
         thread->exprv[thread->exprc - 2]->procedure->operator != op_bind) {
        vm_signal_error(thread, VM_ERROR_INTERNAL);
        vm_set_error_string(thread, "illicit definition context");
        return;
      }

      /* Extend the bind vector by one element. */
      bindv = VM_REALLOC(thread->expr->bindv,
                         sizeof(vm_symbol_bind_t) * thread->expr->bindc);
      if(bindv == NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
      thread->expr->bindv = bindv;

      thread->exprc--;
      vm_symbol_bind(thread, &argv[0].value.symbol_ref,
                     &argv[1]);
      thread->exprc++;
    } else {
      /* Definitions at the top level of the executed program have
         global scope. */
      obj = vm_symbol_resolve(thread, &argv[0].value.symbol_ref);
      if(obj != NULL) {
        if(argc > 1) {
          memcpy(obj, &argv[1], sizeof(vm_obj_t));
        } else {
          obj->type = VM_TYPE_NONE;
        }
      } else {
        vm_signal_error(thread, VM_ERROR_SYMBOL_ID);
      }
    }
  }
}

VM_FUNCTION(set)
{
  vm_obj_t *obj;

  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 1);
    return;
  }

  if(argv[0].type != VM_TYPE_SYMBOL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  } else {
    /* Object definition. */
    obj = vm_symbol_resolve(thread, &argv[0].value.symbol_ref);
    if(obj == NULL) {
      vm_signal_error(thread, VM_ERROR_SYMBOL_ID);
    } else if(IS_CLEAR(thread->program->flags, VM_PROGRAM_FLAG_STRICT_ASSIGNMENT) ||
              obj->type != VM_TYPE_NONE) {
      /* When strict assignment is configured, set! is only allowed if the
         symbol has been bound to an object location earlier. In that case,
         the object type cannot be VM_TYPE_NONE. */
      memcpy(obj, &argv[1], sizeof(vm_obj_t));
    }
  }
}

VM_FUNCTION(and)
{
  int last_eval_arg;

  if(argc == 0) {
    VM_PUSH_BOOLEAN(VM_TRUE);
    VM_EVAL_STOP(thread);
    return;
  }

  last_eval_arg = highest_bit_set(thread->expr->eval_completed);
  if(last_eval_arg == argc - 1) {
    /* No more arguments to evaluate. Use the result of the last
       evaluated argument. */
    VM_PUSH(&argv[argc - 1]);
    VM_EVAL_STOP(thread);
  } else if(last_eval_arg == -1) {
    /* We start the evaluation in this case. */
    VM_EVAL_ARG(thread, 0);
  } else {
    /* Stop if the previously evaluated argument is false. */
    if(!object_is_true(&argv[last_eval_arg])) {
      VM_PUSH_BOOLEAN(VM_FALSE);
      VM_EVAL_STOP(thread);
      return;
    }

    if(last_eval_arg + 2 == argc) {
      /* We are about to evaluate the last argument. Allow tail call
         optimization because the evaluated argument will be the
         result of the AND expression. */
      SET(thread->expr->flags, VM_EXPR_TAIL_CALL);
    }
    VM_EVAL_ARG(thread, last_eval_arg + 1);
  }
}

VM_FUNCTION(or)
{
  int last_eval_arg;

  if(argc == 0) {
    VM_PUSH_BOOLEAN(VM_FALSE);
    VM_EVAL_STOP(thread);
    return;
  }

  last_eval_arg = highest_bit_set(thread->expr->eval_completed);
  if(last_eval_arg < 0) {
    VM_EVAL_ARG(thread, 0);
  } else if(object_is_true(&argv[last_eval_arg])) {
    VM_PUSH(&argv[last_eval_arg]);
    VM_EVAL_STOP(thread);
  } else if(last_eval_arg == argc - 1) {
    /* No more arguments to evaluate. Return false. */
    VM_PUSH_BOOLEAN(VM_FALSE);
    VM_EVAL_STOP(thread);
  } else {
    /* There are more arguments, and the previously evaluated one is false. */
    VM_EVAL_ARG(thread, last_eval_arg + 1);
  }
}

VM_FUNCTION(apply)
{
  vm_list_t *list;
  vm_list_item_t *item;
  vm_obj_t *argp;

  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 1);
    return;
  }

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  /* Replace the current expression with the operator and the arguments
     specified by the APPLY expression. */

  memcpy(&thread->expr->argv[0], &argv[0], sizeof(vm_obj_t));
  list = argv[1].value.list;

  if(1 + list->length > VM_OBJECT_STACK_SIZE) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(argp = &thread->expr->argv[1], item = list->head;
      item != NULL;
      argp++, item = item->next) {
    memcpy(argp, &item->obj, sizeof(vm_obj_t));
  }

  thread->expr->argc = 1 + list->length;
  thread->expr->eval_requested = 1;
  thread->expr->eval_completed = 0;

  vm_list_destroy(list);
}

VM_FUNCTION(quote)
{
  VM_PUSH(&argv[0]);
}

VM_FUNCTION(call_with_cc)
{
  vm_expr_t *expr;

  VM_PRINTF("Call CC\n");

  expr = vm_thread_stack_alloc(thread);
  if(expr == NULL) {
    return;
  }
  memcpy(&expr->argv[0], &argv[1], sizeof(vm_obj_t));
  expr->flags = VM_EXPR_HAVE_OBJECTS | VM_EXPR_CONTINUATION |
                VM_EXPR_SAVE_FRAME;
  expr->argc = 2;
  expr->eval_requested = 1;
  expr->eval_completed = 0;

  thread->exprv[thread->exprc++] = expr;
  VM_PRINTF("Call CC cur expr %p, new expr %p\n", thread->expr, expr);
  thread->expr = expr;
}

VM_FUNCTION(values)
{
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
}

VM_FUNCTION(call_with_values)
{
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
}

VM_FUNCTION(dynamic_wind)
{
  int i;

  for(i = 0; i < 3; i++) {
    if(!VM_EVAL_ARG_DONE(thread, i)) {
      VM_EVAL_ARG(thread, i);
      return;
    }
  }

  VM_PUSH(&argv[1]);
}

VM_FUNCTION(eval)
{
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
}

VM_FUNCTION(numberp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_INTEGER  ||
		  argv->type == VM_TYPE_RATIONAL ||
                  argv->type == VM_TYPE_REAL     ||
                  argv->type == VM_TYPE_COMPLEX);
}

VM_FUNCTION(integerp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_INTEGER);
}

VM_FUNCTION(rationalp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_INTEGER ||
                  argv->type == VM_TYPE_RATIONAL);
}

VM_FUNCTION(realp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_INTEGER  ||
                  argv->type == VM_TYPE_RATIONAL ||
                  argv->type == VM_TYPE_REAL);
}

VM_FUNCTION(complexp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_INTEGER  ||
                  argv->type == VM_TYPE_RATIONAL ||
                  argv->type == VM_TYPE_REAL     ||
                  argv->type == VM_TYPE_COMPLEX);
}

VM_FUNCTION(exactp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_INTEGER ||
                  argv->type == VM_TYPE_RATIONAL);
}

VM_FUNCTION(inexactp)
{
  VM_PUSH_BOOLEAN(VM_FALSE);
}


VM_FUNCTION(procedurep)
{
  if(argv[0].type == VM_TYPE_FORM &&
     argv[0].value.form.type != VM_FORM_LAMBDA) {
    /* The argument is a regular form, so we need to evaluate it. */
    VM_EVAL_ARG(thread, 0);
  } else {
    VM_PUSH_BOOLEAN(vm_is_procedure(thread, &argv[0]));
    VM_EVAL_STOP(thread);
  }
}

VM_FUNCTION(booleanp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_BOOLEAN);
}

VM_FUNCTION(portp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_PORT);
}

VM_FUNCTION(not)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_BOOLEAN ?
		  !argv->value.boolean : VM_FALSE);
}

VM_FUNCTION(eqp)
{
  VM_PUSH_BOOLEAN(vm_objects_equal(thread, &argv[0], &argv[1]));
}

VM_FUNCTION(eqvp)
{
  VM_PUSH_BOOLEAN(vm_objects_equal(thread, &argv[0], &argv[1]));
}

VM_FUNCTION(equalp)
{
  VM_PUSH_BOOLEAN(vm_objects_deep_equal(thread, &argv[0], &argv[1]));
}
