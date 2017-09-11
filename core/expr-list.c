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

#include "vm-functions.h"
#include "vm-log.h"
#include "vm-list.h"

static void
find_member_in_list(vm_thread_t *thread, vm_integer_t argc, vm_obj_t *argv,
	    int (*compare)(vm_thread_t *, vm_obj_t *, vm_obj_t *))
{
  vm_list_t *list;
  vm_list_item_t *item;
  vm_list_t *result_list;

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;

  for(item = list->head; item != NULL; item = item->next) {
    if(compare(thread, &argv[0], &item->obj)) {
      result_list = vm_list_create();
      if(result_list == NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }

      /* Copy the remainder of the list argument into the
         resulting list. */
      do {
        if(!vm_list_insert_tail(result_list, &item->obj)) {
          vm_list_destroy(result_list);
          vm_signal_error(thread, VM_ERROR_HEAP);
          return;
        }
      } while((item = item->next) != NULL);
      VM_PUSH_LIST(result_list);
      return;
    }
  }

  VM_PUSH_BOOLEAN(VM_FALSE);
}

static void
find_member_in_assoc_list(vm_thread_t *thread,
            vm_integer_t argc, vm_obj_t *argv,
	    int (*compare)(vm_thread_t *, vm_obj_t *, vm_obj_t *))
{
  vm_list_t *list;
  vm_list_item_t *item;
  vm_list_t *assoc_pair;

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;

  for(item = list->head; item != NULL; item = item->next) {
    if(item->obj.type != VM_TYPE_LIST) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    assoc_pair = item->obj.value.list;

    if(compare(thread, &argv[0], &assoc_pair->head->obj)) {
      VM_PUSH_LIST(assoc_pair);
      return;
    }
  }

  VM_PUSH_BOOLEAN(VM_FALSE);
}

VM_FUNCTION(list)
{
  vm_list_t *list;
  vm_integer_t i;

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    for(i = argc - 1; i >= 0; i--) {
      if(!vm_list_insert_head(list, &argv[i])) {
	vm_signal_error(thread, VM_ERROR_HEAP);
	return;
      }
    }
    
    thread->result.type = VM_TYPE_LIST;
    thread->result.value.list = list;
  }
}

VM_FUNCTION(cons)
{
  vm_list_t *list;

  if(argv[1].type == VM_TYPE_LIST) {
    list = vm_list_copy(argv[1].value.list);
    if(list == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  } else {
    list = vm_list_create();
    if(list == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    SET(list->flags, VM_LIST_FLAG_PAIR);
    if(!vm_list_insert_head(list, &argv[1])) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  if(!vm_list_insert_head(list, &argv[0])) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  VM_PUSH_LIST(list);
}

VM_FUNCTION(push)
{
  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  } else {
    if(vm_list_insert_head(argv[1].value.list, &argv[0]) == VM_FALSE) {
      vm_signal_error(thread, VM_ERROR_HEAP);
    }
  }
}

VM_FUNCTION(pop)
{
  vm_list_t *stack;

  stack = argv[0].value.list;
  if(stack->length == 0) {
    vm_signal_error(thread, VM_ERROR_STACK_UNDERFLOW);
    return;
  }

  VM_PUSH(&stack->head->obj);
  stack->length--;
  stack->head = stack->head->next;
}

VM_FUNCTION(car)
{
  vm_obj_t *obj;

  obj = vm_list_car(argv[0].value.list);
  if(obj == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  } else {
    VM_PUSH(obj);
  }
}

VM_FUNCTION(cdr)
{
  vm_list_t *list;

  list = argv->value.list;
  if(IS_SET(list->flags, VM_LIST_FLAG_PAIR)) {
    if(list->length != 2) {
      vm_signal_error(thread, VM_ERROR_INTERNAL);
    } else {
      VM_PUSH(&list->head->next->obj);
    }
    return;
  }

  list = vm_list_cdr(argv->value.list, 1);
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  } else {
    thread->result.type = VM_TYPE_LIST;
    thread->result.value.list = list;
  }
}

VM_FUNCTION(list_ref)
{
  vm_list_t *list;
  vm_integer_t k;
  vm_obj_t *obj;

  if(argv[0].type != VM_TYPE_LIST || argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[0].value.list;
  k = argv[1].value.integer;

  obj = vm_list_nth(list, k);
  if(obj == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
  } else {
    VM_PUSH(obj);
  }
}

VM_FUNCTION(list_tail)
{
  vm_list_t *list;
  vm_list_item_t *item;
  vm_integer_t k;
  vm_obj_t result;

  if(argv[0].type != VM_TYPE_LIST || argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[0].value.list;
  k = argv[1].value.integer;

  if(k < 0 || list->length < k) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
    return;
  }

  for(item = list->head; k > 0; k--, item = item->next);

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    list->head = item;
    list->length = list->length - k;
    list->flags = 0;

    result.type = VM_TYPE_LIST;
    result.value.list = list;
    VM_PUSH(&result);
  }
}

VM_FUNCTION(append)
{
  vm_list_t *list, *new_list;
  vm_obj_t *obj;

  /* TODO: Fix the bug with arguments that are empty lists. */
  if(argc == 2) {
    /* Initiate the APPEND operation. */
    argv[3].type = VM_TYPE_LIST;
    new_list = argv[3].value.list = vm_list_copy(argv[0].value.list);
    if(new_list == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Create a new argument that stores the processed list and the result. */
    thread->expr->argc = 4;
    argv[2].type = VM_TYPE_LIST;
    argv[2].value.list = argv[1].value.list;
  } else {
    new_list = argv[3].value.list;
  }

  list = argv[2].value.list;
  if(list->length == 0) {
    /* The list has been processed. Stop the evaluation. */
    memcpy(&thread->result, &argv[3], sizeof(vm_obj_t));
    thread->expr->argc = 2;
    return;
  }

  obj = vm_list_car(list);
  if(obj == NULL) {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
    return;
  }

  if(!vm_list_insert_tail(new_list, obj)) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  /* Continue the APPEND operation. */
  list = vm_list_cdr(list, 0);
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    argv[2].value.list = list;
    VM_EVAL_SET_REQUESTED(thread, 0);
    thread->expr->eval_completed = 0;
  }
}

VM_FUNCTION(remove)
{
  vm_list_t *list;
  vm_list_item_t *prev_item;
  vm_list_item_t *item;

  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[1].value.list;

  prev_item = NULL;
  item = list->head;
  while(item != NULL) {
    if(vm_objects_deep_equal(thread, &item->obj, &argv[0])) {
      list->length--;

      if(item != list->head && item != list->tail) {
        prev_item->next = item->next;
      } else {
        if(item == list->tail) {
          list->tail = prev_item;
          if(prev_item != NULL) {
            prev_item->next = NULL;
          }
        }
        if(item == list->head) {
          list->head = item->next;
        }
      }
    }

    prev_item = item;
    item = item->next;
  }
}

VM_FUNCTION(reverse)
{
  vm_list_t *list;
  vm_list_item_t *item;

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(item = argv[0].value.list->head; item != NULL; item = item->next) {
    if(!vm_list_insert_head(list, &item->obj)) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      vm_list_destroy(list);
      return;
    }
  }

  VM_PUSH_LIST(list);
}

VM_FUNCTION(length)
{
  VM_PUSH_INTEGER(argv->value.list->length);
}

VM_FUNCTION(nullp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_LIST &&
                  argv[0].value.list->length == 0);
}

VM_FUNCTION(listp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_LIST &&
                  IS_CLEAR(argv[0].value.list->flags, VM_LIST_FLAG_PAIR));
}

VM_FUNCTION(pairp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_LIST &&
                  IS_SET(argv[0].value.list->flags, VM_LIST_FLAG_PAIR));
}

VM_FUNCTION(set_car)
{
  vm_list_t *list;

  if(argv[0].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list = argv[0].value.list;
  if(list->length == 0) {
    if(vm_list_insert_head(list, &argv[1]) == VM_FALSE) {
      vm_signal_error(thread, VM_ERROR_HEAP);
    }
  } else {
    memcpy(&list->head->obj, &argv[1], sizeof(vm_obj_t));
  }
}

VM_FUNCTION(set_cdr)
{
  if(argv[0].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  if(!vm_list_set_cdr(argv[0].value.list, &argv[1])) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  }
}

VM_FUNCTION(memq)
{
  find_member_in_list(thread, argc, argv, vm_objects_equal);
}

VM_FUNCTION(memv)
{
  find_member_in_list(thread, argc, argv, vm_objects_equal);
}

VM_FUNCTION(member)
{
  find_member_in_list(thread, argc, argv, vm_objects_deep_equal);
}

VM_FUNCTION(assq)
{
  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 0);
    VM_EVAL_ARG(thread, 1);
  } else {
    find_member_in_assoc_list(thread, argc, argv, vm_objects_equal);
  }
}

VM_FUNCTION(assv)
{
  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 0);
    VM_EVAL_ARG(thread, 1);
  } else {
    find_member_in_assoc_list(thread, argc, argv, vm_objects_equal);
  }
}

VM_FUNCTION(assoc)
{
  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 0);
    VM_EVAL_ARG(thread, 1);
  } else {
    find_member_in_assoc_list(thread, argc, argv, vm_objects_deep_equal);
  }
}
