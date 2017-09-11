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

#include "vm-functions.h"
#include "vm-log.h"

static int
raise_exception(vm_thread_t *thread, vm_error_type_t error_type)
{
  vm_obj_t obj;
  const char *symbol_name;
  const char *errors_to_symbols[] =
    {
      NULL,               /* VM_ERROR_INTERNAL */
      "MemoryException",  /* VM_ERROR_HEAP */
      NULL,               /* VM_ERROR_BYTECODE */
      NULL,               /* VM_ERROR_TOKEN */
      NULL,               /* VM_ERROR_ARGUMENT_COUNT */
      "TypeException",    /* VM_ERROR_ARGUMENT_TYPES */
      "ValueException",   /* VM_ERROR_ARGUMENT_VALUE */
      NULL,               /* VM_ERROR_EXPR_ID */
      NULL,               /* VM_ERROR_STRING_ID */
      NULL,               /* VM_ERROR_SYMBOL_ID */
      NULL,               /* VM_ERROR_UNDEFINED */
      "ThreadException",  /* VM_ERROR_THREAD */
      NULL,               /* VM_ERROR_STACK_OVERFLOW */
      NULL,               /* VM_ERROR_STACK_UNDERFLOW */
      "Div0Exception",    /* VM_ERROR_DIV0 */
      NULL,               /* VM_ERROR_UNIMPLEMENTED */
      "IOException",      /* VM_ERROR_WRITE_PROHIBITED */
      "IOException",      /* VM_ERROR_IO */
      "IOException",      /* VM_ERROR_PORT_CLOSED */
      "SocketException",  /* VM_ERROR_SOCKET */
      "PolicyException",  /* VM_ERROR_POLICY */
      "NameException",    /* VM_ERROR_NAME */
      NULL,               /* VM_ERROR_UNHANDLED_EXCEPTION */
      "LibraryException"  /* VM_ERROR_LIBRARY */
      "SyscallException"  /* VM_ERRROR_SYSCALL */
    };

  obj.type = VM_TYPE_SYMBOL;

  if(error_type >= sizeof(errors_to_symbols) / sizeof(errors_to_symbols[0])) {
    return 0;
  }

  symbol_name = errors_to_symbols[error_type];
  if(symbol_name == NULL ||
     vm_symbol_get_ref(thread, symbol_name, &obj.value.symbol_ref) == 0) {
    return 0;
  }

  VM_DEBUG(VM_DEBUG_LOW, "Raise exception %s", symbol_name);
  vm_raise_exception(thread, &obj);
  return 1;
}

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
void
_vm_signal_error(vm_thread_t *thread, vm_error_type_t error_type,
                 const char *file, const unsigned line)
{
  if(thread == NULL || raise_exception(thread, error_type)) {
    return;
  }

  /* Do not overwrite errors. */
  if(thread->status != VM_THREAD_ERROR) {
    thread->status = VM_THREAD_ERROR;
    thread->error.error_ip = thread->expr->ip;
    thread->error.error_type = error_type;
    thread->error.file = file;
    thread->error.line = line;
  }
}
#else
void
_vm_signal_error(vm_thread_t *thread, vm_error_type_t error_type)
{
  if(thread == NULL || raise_exception(thread, error_type)) {
    return;
  }

  /* Do not overwrite errors. */
  if(thread->status != VM_THREAD_ERROR) {
    thread->status = VM_THREAD_ERROR;
    thread->error.error_ip = thread->expr->ip;
    thread->error.error_type = error_type;
  }
}
#endif /* VM_DEBUG */

void
vm_set_error_object(vm_thread_t *thread, vm_obj_t *obj)
{
  if(thread != NULL && obj != NULL) {
    memcpy(&thread->error.error_obj, obj, sizeof(vm_obj_t));
  }
}

void
vm_set_error_string(vm_thread_t *thread, const char *str)
{
  if(thread != NULL) {
    vm_string_create(&thread->error.error_obj, -1, str);
  }
}

void
vm_raise_exception(vm_thread_t *thread, vm_obj_t *obj)
{
  int i;
  int j;

  for(i = thread->exprc - 1; i >= 0; i--) {
    if(thread->exprv[i]->procedure != NULL &&
       thread->exprv[i]->procedure->operator == op_guard) {
      /* Prepare a jump to the guard expression. */
      thread->exprc = i + 1;
      thread->expr = thread->exprv[i];

      thread->expr->bindv = VM_MALLOC(sizeof(vm_symbol_bind_t));
      if(thread->expr->bindv == NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }

      /* Set the guard object to the argument of the raise expression. */
      vm_symbol_bind(thread, &thread->exprv[i]->argv[1].value.symbol_ref, obj);

      /* Deallocate the stack above that of the guard expression. */
      for(j = thread->exprc - 1; j > i; j--) {
        vm_thread_stack_free(thread->exprv[j]);
      }

      /* Evaluate the guard handler. */
      VM_EVAL_SET_REQUESTED(thread, 2);
      VM_EVAL_SET_COMPLETED(thread, 3);

      return;
    }
  }

  vm_signal_error(thread, VM_ERROR_UNHANDLED_EXCEPTION);
  vm_set_error_object(thread, obj);
}

void
vm_return_from_function(vm_thread_t *thread, vm_obj_t *obj)
{
  int i;
  int j;

  for(i = thread->exprc - 1; i >= 0; i--) {
    if(thread->exprv[i]->procedure != NULL &&
       thread->exprv[i]->procedure->operator == op_bind) {
      /* Prepare a jump to the expression that called the function, which
         will be at one level above the BIND expression. */
      thread->exprc = i;
      thread->expr = thread->exprv[i - 1];
      VM_EVAL_SET_COMPLETED(thread, thread->expr->eval_arg);

      VM_DEBUG(VM_DEBUG_HIGH, "Return to frame %d, arg %d\n",
               i - 1, thread->expr->eval_arg);

      if(obj != NULL) {
        /* An argument was supplied to be passed as the result of the
           function call from which we return. */
        memcpy(&thread->expr->argv[thread->expr->eval_arg++],
               obj, sizeof(vm_obj_t));
      }

      /* Deallocate the stack above that of the bind expression. */
      for(j = thread->exprc - 1; j > i; j--) {
        vm_thread_stack_free(thread->exprv[j]);
      }

      return;
    }
  }

  vm_signal_error(thread, VM_ERROR_UNHANDLED_EXCEPTION);
  vm_set_error_object(thread, obj);
}

void
vm_write_object(vm_port_t *port, vm_obj_t *obj)
{
  static uint8_t nested_print;
  vm_thread_t *thread;
  const char *symbol_name;
  const char *str;
  vm_list_item_t *item;
  int i;
  vm_boolean_t output_raw;

  if(port != NULL && port->thread != NULL) {
    thread = port->thread;
  } else {
    thread = vm_current_thread();
  }

  switch(obj->type) {
  case VM_TYPE_NONE:
    vm_write(port, "#<unspecified>");
    break;
  case VM_TYPE_BOOLEAN:
    vm_write(port, "%s", obj->value.boolean == 0 ? "#f" : "#t");
    break;
  case VM_TYPE_INTEGER:
    vm_write(port, "%ld", (long)obj->value.integer);
    break;
  case VM_TYPE_RATIONAL:
    vm_write(port, "%ld", (long)obj->value.rational->numerator);
    if(obj->value.rational->denominator != 1) {
      vm_write(port, "/%ld", (long)obj->value.rational->denominator);
    }
    break;
  case VM_TYPE_STRING:
    str = vm_string_resolve(thread, obj->value.string);
    vm_write(port, nested_print ? "\"%s\"" : "%s",
                        str == NULL ? "<?>" : str);
    break;
  case VM_TYPE_SYMBOL:
    symbol_name = vm_symbol_lookup(thread != NULL ? thread->program : NULL,
                                   &obj->value.symbol_ref);
    vm_write(port, "%s", symbol_name == NULL ? "<?>" : symbol_name);
    break;
  case VM_TYPE_FORM:
    vm_write(port, "(<%sform %u>)",
	     obj->value.form.type == VM_FORM_LAMBDA ? "lambda " : "",
	     (unsigned)obj->value.form.id);
    break;
  case VM_TYPE_CHARACTER:
    vm_write(port, "%c", (char)obj->value.character);
    break;
#if VM_ENABLE_REALS
  case VM_TYPE_REAL:
    vm_write(port, "%f", (float)obj->value.real);
    break;
#endif /* VM_ENABLED_REALS */
  case VM_TYPE_LIST:
    nested_print = 1;
    vm_write(port, "(");
    for(i = 0, item = obj->value.list->head; item != NULL; i++) {
      if(item->next == NULL &&
         IS_SET(obj->value.list->flags, VM_LIST_FLAG_PAIR)) {
        vm_write(port, ". ");
      }
      vm_write_object(port, (vm_obj_t *)&item->obj);
      item = item->next;

      if(item != NULL) {
        vm_write(port, " ");
        if(i + 1 == VM_LIST_PRINT_LIMIT) {
          vm_write(port, "<%u items omitted>",
		   (unsigned)(obj->value.list->length - i - 1));
          break;
        }
      }
    }
    vm_write(port, ")");
    nested_print = 0;
    break;
  case VM_TYPE_VECTOR:
    if(IS_SET(obj->value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
      output_raw = port != NULL &&
                   IS_CLEAR(port->flags, VM_PORT_FLAG_CONSOLE);

      if(output_raw) {
        vm_native_write_buffer(port, (const char *)obj->value.vector->bytes,
                               obj->value.vector->length);
      } else {
        for(i = 0; i < obj->value.vector->length; i++) {
	  vm_write(port, "\\x%02x", obj->value.vector->bytes[i]);
        }
      }
    } else {
      nested_print = 1;
      vm_write(port, "#(");
      for(i = 0; i < obj->value.vector->length; i++) {
        vm_write_object(port, &obj->value.vector->elements[i]);

	if(i < obj->value.vector->length - 1) {
	  vm_write(port, " ");
	  if(i + 1 == VM_LIST_PRINT_LIMIT) {
	    vm_write(port, "<%u items omitted>",
		     (unsigned)(obj->value.vector->length - i - 1));
	    break;
	  }
	}
      }
      vm_write(port, ")");
      nested_print = 0;
    }
    break;
  case VM_TYPE_PORT:
    vm_write(port, "#(port %d)", obj->value.port->fd);
    break;
  case VM_TYPE_PROCEDURE:
    vm_write(port, "#(libfunc %p)", obj->value.procedure);
    break;
  case VM_TYPE_EXTERNAL:
    obj->value.ext_object.type->write(port, obj);
    break;
  default:
    vm_write(port, "#<unknown>");
  }
}

int
vm_is_procedure(vm_thread_t *thread, vm_obj_t *obj)
{
  if(obj->type == VM_TYPE_SYMBOL) {
    return vm_procedure_lookup(thread->program, &obj->value.symbol_ref) != NULL;
  }

  return (obj->type == VM_TYPE_FORM &&
          obj->value.form.type == VM_FORM_LAMBDA) ||
         obj->type == VM_TYPE_PROCEDURE;
}

int
vm_objects_equal(vm_thread_t *thread, vm_obj_t *obj1, vm_obj_t *obj2)
{
  vm_string_t *string1, *string2;
  const char *str1, *str2;

  if(obj1->type != obj2->type) {
    return 0;
  }

  switch(obj1->type) {
  case VM_TYPE_BOOLEAN:
    return obj1->value.boolean == obj2->value.boolean;
  case VM_TYPE_INTEGER:
    return obj1->value.integer == obj2->value.integer;
  case VM_TYPE_RATIONAL:
    return memcmp(&obj1->value.rational, &obj2->value.rational,
                  sizeof(vm_rational_t)) == 0;
  case VM_TYPE_STRING:
    string1 = obj1->value.string;
    string2 = obj2->value.string;

    str1 = vm_string_resolve(thread, string1);
    str2 = vm_string_resolve(thread, string2);
    if(str1 == NULL || str2 == NULL) {
      return 0;
    }
    return strcmp(str1, str2) == 0;
  case VM_TYPE_SYMBOL:
    return obj1->value.symbol_ref.scope == obj2->value.symbol_ref.scope &&
      obj1->value.symbol_ref.symbol_id == obj2->value.symbol_ref.symbol_id;
  case VM_TYPE_FORM:
    return obj1->value.form.id == obj2->value.form.id;
  case VM_TYPE_CHARACTER:
    return obj1->value.character == obj2->value.character;
#if VM_ENABLE_REALS
  case VM_TYPE_REAL:
    return obj1->value.real == obj2->value.real;
#endif
  case VM_TYPE_LIST:
    return obj1->value.list == obj2->value.list ||
      (obj1->value.list->length == 0 && obj2->value.list->length == 0);
  case VM_TYPE_VECTOR:
    return obj1->value.vector->length == obj2->value.vector->length &&
      obj1->value.vector->elements == obj2->value.vector->elements;
  case VM_TYPE_PORT:
    return memcmp(&obj1->value.port, &obj2->value.port,
                  sizeof(vm_port_t)) == 0;
  case VM_TYPE_PROCEDURE:
    return obj1->value.procedure == obj2->value.procedure;
  case VM_TYPE_EXTERNAL:
    return obj1->value.ext_object.type == obj2->value.ext_object.type &&
      obj1->value.ext_object.opaque_data == obj2->value.ext_object.opaque_data;
  default:
    break;
  }

  return 0;
}

int
vm_objects_deep_equal(vm_thread_t *thread, vm_obj_t *obj1, vm_obj_t *obj2)
{
  vm_list_t *list1, *list2;
  vm_list_item_t *item1, *item2;
  vm_vector_t *vector1, *vector2;
  int i;

  if(obj1->type != obj2->type) {
    return 0;
  }

  if(obj1->type == VM_TYPE_LIST) {
    list1 = obj1->value.list;
    list2 = obj2->value.list;

    if(list1->length != list2->length) {
      return 0;
    }

    item1 = list1->head;
    item2 = list2->head;
    while(item1 != NULL && item2 != NULL) {
      if(!vm_objects_deep_equal(thread, &item1->obj, &item2->obj)) {
        return 0;
      }
      item1 = item1->next;
      item2 = item2->next;
    }

    return 1;
  } else if(obj1->type == VM_TYPE_VECTOR) {
    vector1 = obj1->value.vector;
    vector2 = obj2->value.vector;

    if(vector1->length != vector2->length) {
      return 0;
    }

    for(i = 0; i < vector1->length; i++) {
      if(!vm_objects_deep_equal(thread, &vector1->elements[i],
                                &vector2->elements[i])) {
	return 0;
      }
    }

    return 1;
  } else {
    return vm_objects_equal(thread, obj1, obj2);
  }
}

int
vm_object_deep_copy(vm_obj_t *old, vm_obj_t *new)
{
  memcpy(new, old, sizeof(vm_obj_t));

  switch(old->type) {
  case VM_TYPE_LIST:
    break;
  case VM_TYPE_VECTOR:
    break;
  case VM_TYPE_EXTERNAL:
    old->value.ext_object.type->copy(new, old);
    break;
  default:
    break;
  }

  return 1;
}
