/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB.
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
 */

#include "vm.h"
#include "vm-pair.h"

int
vm_list_walker_init(vm_list_walker_t *w, const vm_obj_t *obj)
{
  if(obj->type == VM_TYPE_LIST) {
    w->kind = VM_TYPE_LIST;
    w->list_root = obj->value.list;
    w->list_item = obj->value.list != NULL ? obj->value.list->head : NULL;
    w->pair_cur.type = VM_TYPE_LIST;
    w->pair_cur.value.list = obj->value.list;
    w->suffix_obj = *obj;
    return 1;
  }
  if(obj->type == VM_TYPE_PAIR) {
    w->kind = VM_TYPE_PAIR;
    w->list_root = NULL;
    w->list_item = NULL;
    w->pair_cur = *obj;
    w->suffix_obj = *obj;
    return 1;
  }
  return 0;
}

int
vm_list_walker_next(vm_list_walker_t *w, vm_obj_t **car_out)
{
  /* Pair walk may transition into a list walk when a pair's cdr is
     a non-empty VM_TYPE_LIST (typical of (cons x '(a b c))): we
     finish walking the list section to satisfy the structural
     definition of a list, regardless of how it was internally
     built. The reverse transition (LIST → PAIR) is not currently
     possible because vm_list_item_t chains never embed pairs. */
  if(w->kind == VM_TYPE_PAIR) {
    if(w->pair_cur.type == VM_TYPE_PAIR && w->pair_cur.value.pair != NULL) {
      w->suffix_obj = w->pair_cur;
      *car_out = &w->pair_cur.value.pair->car;
      w->pair_cur = w->pair_cur.value.pair->cdr;
      return 1;
    }
    /* pair_cur is the terminator. If it's a non-empty VM_TYPE_LIST,
       continue the walk through that list. */
    if(w->pair_cur.type == VM_TYPE_LIST &&
       w->pair_cur.value.list != NULL &&
       w->pair_cur.value.list->length > 0) {
      w->kind = VM_TYPE_LIST;
      w->list_root = w->pair_cur.value.list;
      w->list_item = w->list_root->head;
      /* Fall through to the LIST handling below. */
    } else {
      /* Genuine terminator. Proper if empty list or NULL pair. */
      if(w->pair_cur.type == VM_TYPE_LIST &&
         w->pair_cur.value.list != NULL &&
         w->pair_cur.value.list->length == 0) {
        return 0;
      }
      if(w->pair_cur.type == VM_TYPE_PAIR &&
         w->pair_cur.value.pair == NULL) {
        return 0;
      }
      return -1;
    }
  }

  /* VM_TYPE_LIST walk. Reached the end of the item chain when
     list_item == NULL; terminator is the empty list (proper). */
  if(w->list_item == NULL) {
    return 0;
  }
  w->suffix_obj.type = VM_TYPE_LIST;
  w->suffix_obj.value.list = w->list_root;
  *car_out = &w->list_item->obj;
  w->list_item = w->list_item->next;
  return 1;
}

void
vm_list_walker_terminator(const vm_list_walker_t *w, vm_obj_t *out)
{
  if(w->kind == VM_TYPE_LIST) {
    /* The empty list is represented as a VM_TYPE_LIST of length 0;
       for a partially-walked list there isn't a distinct "tail
       sublist" object, so synthesize the empty-list-of-the-input
       which is sufficient for callers that just need to detect
       proper vs improper. */
    out->type = VM_TYPE_LIST;
    out->value.list = w->list_root;
    /* TODO: this returns the original list, not the empty-tail. The
       walker isn't currently used by primitives that distinguish. */
    return;
  }
  *out = w->pair_cur;
}

void
vm_list_walker_current(const vm_list_walker_t *w, vm_obj_t *out)
{
  *out = w->suffix_obj;
}

int
vm_list_length_walk(const vm_obj_t *obj, vm_integer_t *length_out)
{
  vm_list_walker_t w;
  vm_obj_t *car;
  vm_integer_t n;
  int status;

  /* Fast path for VM_TYPE_LIST: length cached. */
  if(obj->type == VM_TYPE_LIST) {
    *length_out = obj->value.list != NULL ? obj->value.list->length : 0;
    return 0;
  }

  if(!vm_list_walker_init(&w, obj)) {
    return -1;
  }
  n = 0;
  while((status = vm_list_walker_next(&w, &car)) == 1) {
    n++;
  }
  *length_out = n;
  return status < 0 ? 1 : 0;
}

int
vm_obj_is_pair(const vm_obj_t *obj)
{
  if(obj->type == VM_TYPE_PAIR) {
    return obj->value.pair != NULL;
  }
  if(obj->type == VM_TYPE_LIST) {
    return obj->value.list != NULL && obj->value.list->length > 0;
  }
  return 0;
}

int
vm_obj_is_null(const vm_obj_t *obj)
{
  return obj->type == VM_TYPE_LIST &&
         obj->value.list != NULL &&
         obj->value.list->length == 0;
}

int
vm_obj_is_proper_list(const vm_obj_t *obj)
{
  vm_list_walker_t w;
  vm_obj_t *car;
  int status;

  if(obj->type == VM_TYPE_LIST) {
    return obj->value.list != NULL &&
           VM_IS_CLEAR(obj->value.list->flags, VM_LIST_FLAG_PAIR);
  }
  if(obj->type != VM_TYPE_PAIR) {
    return 0;
  }
  if(!vm_list_walker_init(&w, obj)) {
    return 0;
  }
  while((status = vm_list_walker_next(&w, &car)) == 1) {
    /* iterate to terminator */
  }
  return status == 0;
}
