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
      /* Disable GC during list construction */
      vm_gc_disable();

      result_list = vm_list_create();
      if(result_list == NULL) {
        vm_gc_enable();
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }

      /* Copy the remainder of the list argument into the
         resulting list. */
      do {
        if(!vm_list_insert_tail(result_list, &item->obj)) {
          vm_gc_enable();
          vm_list_destroy(result_list);
          vm_signal_error(thread, VM_ERROR_HEAP);
          return;
        }
      } while((item = item->next) != NULL);

      vm_gc_enable();
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

  /* Disable GC during list construction */
  vm_gc_disable();

  list = vm_list_create();
  if(list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    for(i = argc - 1; i >= 0; i--) {
      if(!vm_list_insert_head(list, &argv[i])) {
	vm_gc_enable();
	vm_signal_error(thread, VM_ERROR_HEAP);
	return;
      }
    }

    vm_gc_enable();
    thread->result.type = VM_TYPE_LIST;
    thread->result.value.list = list;
  }
}

VM_FUNCTION(cons)
{
  vm_list_t *list;

  /* Disable GC during list construction */
  vm_gc_disable();

  if(argv[1].type == VM_TYPE_LIST) {
    list = vm_list_copy(argv[1].value.list);
    if(list == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  } else {
    list = vm_list_create();
    if(list == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    VM_SET_FLAG(list->flags, VM_LIST_FLAG_PAIR);
    if(!vm_list_insert_head(list, &argv[1])) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  if(!vm_list_insert_head(list, &argv[0])) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  vm_gc_enable();
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
  if(VM_IS_SET(list->flags, VM_LIST_FLAG_PAIR)) {
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
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    vm_set_error_object(thread, &argv[1]);
    return;
  }

  /* Save k before the loop modifies it */
  vm_integer_t orig_k = k;

  for(item = list->head; k > 0; k--, item = item->next);

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    vm_list_item_t *tail_item;

    list->head = item;
    list->length = argv[0].value.list->length - orig_k;  /* Use saved k value */
    list->flags = 0;

    /* Find and set the tail pointer */
    if(item != NULL) {
      tail_item = item;
      while(tail_item->next != NULL) {
        tail_item = tail_item->next;
      }
      list->tail = tail_item;
    } else {
      list->tail = NULL;
    }

    result.type = VM_TYPE_LIST;
    result.value.list = list;
    VM_PUSH(&result);
  }
}

VM_FUNCTION(slice)
{
  vm_integer_t start;
  vm_integer_t end;
  vm_integer_t length;

  if(argv[1].type != VM_TYPE_INTEGER || argv[2].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  start = argv[1].value.integer;
  end = argv[2].value.integer;

  /* Handle lists, strings, and vectors */
  if(argv[0].type == VM_TYPE_STRING) {
    /* String slicing */
    vm_string_t *string;
    vm_string_t *result_string;

    string = argv[0].value.string;
    length = string->length;

    /* Handle negative indices (Python-style) */
    if(start < 0) {
      start = length + start;
      if(start < 0) {
        start = 0;
      }
    }

    if(end < 0) {
      end = length + end;
      if(end < 0) {
        end = 0;
      }
    }

    /* Clamp to valid range */
    if(start > length) {
      start = length;
    }
    if(end > length) {
      end = length;
    }

    /* Ensure start <= end */
    if(start > end) {
      start = end;
    }

    /* Create substring */
    result_string = vm_string_create(&thread->result, end - start, NULL);
    if(result_string == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    memcpy(result_string->str, string->str + start, end - start);
    result_string->str[result_string->length] = '\0';
    thread->result.type = VM_TYPE_STRING;
    return;
  } else if(argv[0].type == VM_TYPE_LIST) {
    /* List slicing */
    vm_list_t *input_list;
    vm_list_t *result_list;
    vm_list_item_t *item;
    vm_integer_t i;

    input_list = argv[0].value.list;
    length = input_list->length;

    /* Handle negative indices (Python-style) */
    if(start < 0) {
      start = length + start;
      if(start < 0) {
        start = 0;
      }
    }

    if(end < 0) {
      end = length + end;
      if(end < 0) {
        end = 0;
      }
    }

    /* Clamp to valid range */
    if(start > length) {
      start = length;
    }
    if(end > length) {
      end = length;
    }

    /* Ensure start <= end */
    if(start > end) {
      start = end;
    }

    /* Disable GC during list construction */
    vm_gc_disable();

    /* Create result list */
    result_list = vm_list_create();
    if(result_list == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Skip to start position */
    item = input_list->head;
    for(i = 0; i < start && item != NULL; i++) {
      item = item->next;
    }

    /* Copy elements from start to end */
    for(i = start; i < end && item != NULL; i++) {
      if(!vm_list_insert_tail(result_list, &item->obj)) {
        vm_gc_enable();
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
      item = item->next;
    }

    vm_gc_enable();
    VM_PUSH_LIST(result_list);
  } else if(argv[0].type == VM_TYPE_VECTOR) {
    /* Vector slicing */
    vm_vector_t *input_vector;
    vm_vector_t *result_vector;
    vm_integer_t i;

    input_vector = argv[0].value.vector;
    length = input_vector->length;

    /* Handle negative indices (Python-style) */
    if(start < 0) {
      start = length + start;
      if(start < 0) {
        start = 0;
      }
    }

    if(end < 0) {
      end = length + end;
      if(end < 0) {
        end = 0;
      }
    }

    /* Clamp to valid range */
    if(start > length) {
      start = length;
    }
    if(end > length) {
      end = length;
    }

    /* Ensure start <= end */
    if(start > end) {
      start = end;
    }

    /* Create result vector */
    result_vector = vm_vector_create(&thread->result, end - start, VM_VECTOR_FLAG_REGULAR);
    if(result_vector == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Copy elements from start to end */
    for(i = 0; i < end - start; i++) {
      result_vector->elements[i] = input_vector->elements[start + i];
    }

    thread->result.type = VM_TYPE_VECTOR;
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
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

  /* Return the modified list */
  VM_PUSH_LIST(list);
}

VM_FUNCTION(reverse)
{
  vm_list_t *list;
  vm_list_item_t *item;

  /* Disable GC during list construction to prevent premature collection
   * of the partially-built list and its contents */
  vm_gc_disable();

  list = vm_list_create();
  if(list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(item = argv[0].value.list->head; item != NULL; item = item->next) {
    if(!vm_list_insert_head(list, &item->obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      vm_list_destroy(list);
      return;
    }
  }

  /* Re-enable GC before returning */
  vm_gc_enable();
  VM_PUSH_LIST(list);
}

VM_FUNCTION(length)
{
  /* Support both lists and strings */
  if(argv[0].type == VM_TYPE_LIST) {
    VM_PUSH_INTEGER(argv[0].value.list->length);
  } else if(argv[0].type == VM_TYPE_STRING) {
    VM_PUSH_INTEGER(argv[0].value.string->length);
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  }
}

VM_FUNCTION(nullp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_LIST &&
                  argv[0].value.list->length == 0);
}

VM_FUNCTION(listp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_LIST &&
                  VM_IS_CLEAR(argv[0].value.list->flags, VM_LIST_FLAG_PAIR));
}

VM_FUNCTION(pairp)
{
  /* R5RS: pair? returns #t for any cons cell (non-empty list structure).
     This includes both proper lists like '(1 2 3) and improper pairs like (cons 1 2).
     Only the empty list '() is not a pair. */
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_LIST &&
                  argv[0].value.list->length > 0);
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

VM_FUNCTION(list_enumerate)
{
  vm_list_t *input_list;
  vm_list_t *result_list;
  vm_list_t *pair;
  vm_list_item_t *item;
  vm_obj_t index_obj;
  vm_obj_t pair_obj;
  vm_integer_t index;

  /* Validate argument type */
  if(argv[0].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  input_list = argv[0].value.list;

  /* Disable GC during list construction */
  vm_gc_disable();

  /* Create result list */
  result_list = vm_list_create();
  if(result_list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  /* Iterate through input list, creating (index . element) pairs */
  index = 0;
  for(item = input_list->head; item != NULL; item = item->next) {
    /* Create pair (cons index element) */
    pair = vm_list_create();
    if(pair == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Add index as first element */
    index_obj.type = VM_TYPE_INTEGER;
    index_obj.value.integer = index;
    if(!vm_list_insert_tail(pair, &index_obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Add element as second element */
    if(!vm_list_insert_tail(pair, &item->obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Mark as pair */
    VM_SET_FLAG(pair->flags, VM_LIST_FLAG_PAIR);

    /* Add pair to result list */
    pair_obj.type = VM_TYPE_LIST;
    pair_obj.value.list = pair;
    if(!vm_list_insert_tail(result_list, &pair_obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    index++;
  }

  vm_gc_enable();
  VM_PUSH_LIST(result_list);
}

VM_FUNCTION(list_zip)
{
  vm_list_t *list1, *list2;
  vm_list_t *result_list;
  vm_list_t *pair;
  vm_list_item_t *item1, *item2;
  vm_obj_t pair_obj;

  /* Validate argument types */
  if(argv[0].type != VM_TYPE_LIST || argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  list1 = argv[0].value.list;
  list2 = argv[1].value.list;

  /* Disable GC during list construction */
  vm_gc_disable();

  /* Create result list */
  result_list = vm_list_create();
  if(result_list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  /* Iterate through both lists simultaneously, stop at shortest */
  item1 = list1->head;
  item2 = list2->head;
  while(item1 != NULL && item2 != NULL) {
    /* Create pair (cons elem1 elem2) */
    pair = vm_list_create();
    if(pair == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Add first element */
    if(!vm_list_insert_tail(pair, &item1->obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Add second element */
    if(!vm_list_insert_tail(pair, &item2->obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Mark as pair */
    VM_SET_FLAG(pair->flags, VM_LIST_FLAG_PAIR);

    /* Add pair to result list */
    pair_obj.type = VM_TYPE_LIST;
    pair_obj.value.list = pair;
    if(!vm_list_insert_tail(result_list, &pair_obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    item1 = item1->next;
    item2 = item2->next;
  }

  vm_gc_enable();
  VM_PUSH_LIST(result_list);
}

VM_FUNCTION(list_index)
{
  vm_list_t *list;
  vm_list_item_t *item;
  vm_obj_t *target;
  vm_integer_t index;

  /* Validate argument types */
  if(argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  target = &argv[0];
  list = argv[1].value.list;

  /* Search for element in list */
  index = 0;
  for(item = list->head; item != NULL; item = item->next) {
    if(vm_objects_deep_equal(thread, target, &item->obj)) {
      /* Found - return index */
      VM_PUSH_INTEGER(index);
      return;
    }
    index++;
  }

  /* Not found - return -1 */
  VM_PUSH_INTEGER(-1);
}
