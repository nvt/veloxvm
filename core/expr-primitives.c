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
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
  } else {
    /* Evaluation finished. */
    VM_EVAL_STOP(thread);
    VM_PUSH(&argv[argc - 1]);
  }
}

VM_FUNCTION(bind_function)
{
  /* Identical implementation to bind(), but marks function boundaries.
   * This allows the return primitive to distinguish between actual functions
   * and control flow constructs (while loops, let-expansion, etc.).
   * The return primitive will unwind past regular bind frames but stop at
   * bind_function frames.
   */
  vm_expr_t *calling_expr;
  int i;

  if(thread->expr->eval_completed == 1) {
    /* Initiate the evaluation of the bind expression.

       The bind frame needs storage for the formal parameters AND, if
       the caller is a closure, for its captured free variables. A
       no-parameter closure (e.g. (lambda () ...)) still needs the
       captures bound, so we set up bindv whenever any of the two
       contributes a binding -- not only when there are formal params. */

    calling_expr = (thread->exprc >= 2) ?
                   thread->exprv[thread->exprc - 2] : NULL;

    if(argc > 1) {
      if(calling_expr == NULL) {
	/* Bind cannot execute if there is not a calling expression. */
	vm_signal_error(thread, VM_ERROR_BYTECODE);
	return;
      }

      if(calling_expr->argc + 1 != thread->expr->argc) {
	/* There must be enough objects in the calling expression to bind
	   the symbols specified as arguments to the called bind
	   expression. */
	vm_signal_error(thread, VM_ERROR_BYTECODE);
	return;
      }
    }

    {
      unsigned bind_capacity = (argc > 1) ? (unsigned)(argc - 1) : 0;
      if(calling_expr != NULL &&
         calling_expr->argv[0].type == VM_TYPE_CLOSURE) {
        bind_capacity += calling_expr->argv[0].value.closure->capture_count;
      }
      if(bind_capacity > 0) {
        thread->expr->bindv =
          VM_MALLOC(sizeof(vm_symbol_bind_t) * bind_capacity);
        if(thread->expr->bindv == NULL) {
          vm_signal_error(thread, VM_ERROR_HEAP);
          return;
        }
      }
    }

    if(argc > 1) {
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

    /* If the calling expression's operator is a closure, also bind
       the captured free variables. The captures-list (symbol_ids)
       lives in program->captures[form_id]; the captured values live
       in closure->captures, indexed in the same order. */
    if(calling_expr != NULL &&
       calling_expr->argv[0].type == VM_TYPE_CLOSURE) {
      vm_closure_t *closure = calling_expr->argv[0].value.closure;
      if(thread->program->captures != NULL &&
         closure->form_id < thread->program->captures_size &&
         thread->program->captures[closure->form_id] != NULL) {
        vm_captures_t *cap = thread->program->captures[closure->form_id];
        uint8_t k;
        for(k = 0; k < cap->count && k < closure->capture_count; k++) {
          vm_symbol_ref_t ref;
          ref.scope = VM_SYMBOL_SCOPE_APP;
          ref.symbol_id = cap->symbols[k];
          vm_symbol_bind(thread, &ref, &closure->captures[k]);
          if(thread->status == VM_THREAD_ERROR) {
            return;
          }
        }
      }
    }

    /* Instruct the scheduler to evaluate the actual lambda expression. */
    VM_EVAL_ARG(thread, argc - 1);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
  } else {
    /* Evaluation finished.

       If the body's value is a lambda form whose expr_id has captured
       free variables, materialize a closure now -- this frame's
       bindings (formal parameters of the enclosing lambda) are still
       live, so vm_symbol_resolve can snapshot them. Doing this at
       form-read time would be too early; the parameters aren't bound
       yet at that point. */
    VM_EVAL_STOP(thread);
    if(argv[argc - 1].type == VM_TYPE_FORM &&
       argv[argc - 1].value.form.type == VM_FORM_LAMBDA &&
       thread->program->captures != NULL &&
       argv[argc - 1].value.form.id < thread->program->captures_size &&
       thread->program->captures[argv[argc - 1].value.form.id] != NULL) {
      vm_captures_t *cap =
        thread->program->captures[argv[argc - 1].value.form.id];
      vm_expr_id_t form_id = argv[argc - 1].value.form.id;
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
      VM_PUSH(&closure_obj);
    } else {
      VM_PUSH(&argv[argc - 1]);
    }
  }
}

VM_FUNCTION(return)
{
  /* Push the return value to thread->result before unwinding.
     This ensures the value is available after stack manipulation. */
  if(argc > 0) {
    VM_PUSH(&argv[0]);
  }
  vm_return_from_function(thread, argc > 0 ? &argv[0] : NULL);
}

VM_FUNCTION(begin)
{
  /* Defer the evaluation of the arguments in order to set
     the tail call flag when executing the last argument. */
  VM_EVAL_SET_REQUESTED_RANGE(thread, 1, thread->expr->argc);
  VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
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
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
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

    if(!VM_EVAL_ARG_DONE(thread, 1) &&
       ((argv[1].type == VM_TYPE_FORM &&
         argv[1].value.form.type != VM_FORM_LAMBDA) ||
        argv[1].type == VM_TYPE_SYMBOL)) {
      /* Evaluate non-lambda forms and bare symbol references so that
         (define y x) binds y to the value of x rather than to the
         symbol x itself. Lambda forms self-evaluate and integers,
         strings, etc. evaluate to themselves, so they need no extra
         pass through the scheduler. */
      VM_EVAL_ARG(thread, 1);
      return;
    }

    if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_STRICT_ASSIGNMENT) &&
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
    } else if(VM_IS_CLEAR(thread->program->flags, VM_PROGRAM_FLAG_STRICT_ASSIGNMENT) ||
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
      VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
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
    if(last_eval_arg + 2 == argc) {
      /* About to evaluate the last argument -- its result is the result of
         the OR expression, so propagate the tail-call flag. */
      VM_SET_FLAG(thread->expr->flags, VM_EXPR_TAIL_CALL);
    }
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

  if(thread->exprc >= VM_CONTEXT_STACK_SIZE) {
    vm_signal_error(thread, VM_ERROR_STACK_OVERFLOW);
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

  /* Ensure all three arguments are evaluated */
  for(i = 0; i < 3; i++) {
    if(!VM_EVAL_ARG_DONE(thread, i)) {
      VM_EVAL_ARG(thread, i);
      return;
    }
  }

  /* Stub: we evaluate the three argument expressions (typically lambdas)
     but never invoke the resulting procedures, and we push argv[1] -- the
     thunk procedure itself -- as the result rather than the result of
     calling it. TODO: invoke before/thunk/after in sequence, with
     unwind-protect on after and re-entry hooks for continuations. */
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

VM_FUNCTION(symbolp)
{
  VM_PUSH_BOOLEAN(argv->type == VM_TYPE_SYMBOL);
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
