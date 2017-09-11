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

#include <stdlib.h>

#include "vm.h"
#include "vm-list.h"
#include "vm-log.h"

vm_list_t *
vm_list_create(void)
{
  vm_list_t *list;

  list = vm_alloc(sizeof(vm_list_t));
  if(list != NULL) {
    list->head = NULL;
    list->tail = NULL;
    list->length = 0;
    list->flags = VM_LIST_FLAG_ORIGINAL;
  }

  return list;
}

void
vm_list_destroy(vm_list_t *list)
{
  vm_list_item_t *current, *next;

  /* Do not free the items of derived lists (e.g, those
     constructed from a CDR call). */
  if(IS_SET(list->flags, VM_LIST_FLAG_ORIGINAL)) {
    current = list->head;
    while(current != NULL) {
      next = current->next;
      vm_free(current);
      current = next;
    }
  }

  vm_free(list);
}

vm_list_t *
vm_list_copy(vm_list_t *list)
{
  vm_list_t *copy;

  copy = vm_list_create();
  if(copy != NULL) {
    memcpy(copy, list, sizeof(vm_list_t));
  }

  return copy;
}

vm_obj_t *
vm_list_nth(vm_list_t *list, vm_integer_t n)
{
  vm_list_item_t *item;

  if(n < 0 || n > list->length) {
    return NULL;
  }

  for(item = list->head; n > 0; n--, item = item->next);

  return &item->obj;
}

vm_obj_t *
vm_list_car(vm_list_t *list)
{
  if(list == NULL || list->length == 0) {
    return NULL;
  }

  return &list->head->obj;
}

vm_list_t *
vm_list_cdr(vm_list_t *list, int copy)
{
  vm_list_t *cdr_list;

  if(list == NULL || list->length <= 0) {
    return NULL;
  }

  if(copy) {
    cdr_list = vm_alloc(sizeof(vm_list_t));
    if(cdr_list != NULL) {
      cdr_list->head = list->head->next;
      cdr_list->length = list->length - 1;
      cdr_list->flags = list->flags & ~VM_LIST_FLAG_ORIGINAL;
    }
  } else {
    if(list->length == 1) {
      list->head = NULL;
      list->tail = NULL;
      list->length = 0;
    } else {
      vm_free(list->head);
      list->head = list->head->next;
      list->length--;
    }
    cdr_list = list;
  }

  return cdr_list;
}

vm_boolean_t
vm_list_set_cdr(vm_list_t *list, vm_obj_t *obj)
{
  if(list->head == NULL) {
    return VM_FALSE;
  }
  list->head->next = vm_alloc(sizeof(vm_list_item_t));
  if(list->head->next == NULL) {
    return VM_FALSE;
  }

  if(obj->type == VM_TYPE_LIST) {
    if(list == obj->value.list || list->head->next == NULL) {
      /* Do not allow the list to have itself as CDR, or to have
         an empty list as the first argument. */
      return VM_FALSE;
    }
    if(obj->value.list->head == NULL) {
      list->head->next = NULL;
      list->length = 1;
      list->tail = list->head;
    } else {
      memcpy(list->head->next, obj->value.list->head, sizeof(vm_list_item_t));
      list->length = 1 + obj->value.list->length;
      list->tail = obj->value.list->tail;
    }
    CLEAR(list->flags, VM_LIST_FLAG_PAIR);
  } else {
    memcpy(&list->head->next->obj, obj, sizeof(vm_obj_t));
    list->tail = list->head->next;
    list->tail->next = NULL;
    list->length = 2;
    SET(list->flags, VM_LIST_FLAG_PAIR);
  }

  return VM_TRUE;
}

vm_boolean_t
vm_list_insert_head(vm_list_t *list, vm_obj_t *obj)
{
  vm_list_item_t *item;

  item = vm_alloc(sizeof(vm_list_item_t));
  if(item != NULL) {
    memcpy(&item->obj, obj, sizeof(vm_obj_t));
    list->length++;

    item->next = list->head;
    list->head = item;
    if(list->tail == NULL) {
      list->tail = item;
    }
  }

  return item != NULL;
}

vm_boolean_t
vm_list_insert_tail(vm_list_t *list, vm_obj_t *obj)
{
  vm_list_item_t *item;

  item = vm_alloc(sizeof(vm_list_item_t));
  if(item != NULL) {
    memcpy(&item->obj, obj, sizeof(vm_obj_t));
    list->length++;

    item->next = NULL;
    if(list->tail == NULL) {
      list->head = list->tail = item;
    } else {
      list->tail->next = item;
      list->tail = item;
    }
  }

  return item != NULL;
}
