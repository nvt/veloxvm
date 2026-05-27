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
  if(obj->type == VM_TYPE_PAIR || obj->type == VM_TYPE_NIL) {
    /* NIL initializes to "already terminated" so walker_next returns
       0 (clean end) on first call. */
    w->pair_cur = *obj;
    w->suffix_obj = *obj;
    return 1;
  }
  return 0;
}

int
vm_list_walker_next(vm_list_walker_t *w, vm_obj_t **car_out)
{
  if(w->pair_cur.type == VM_TYPE_PAIR && w->pair_cur.value.pair != NULL) {
    w->suffix_obj = w->pair_cur;
    *car_out = &w->pair_cur.value.pair->car;
    w->pair_cur = w->pair_cur.value.pair->cdr;
    return 1;
  }
  /* Terminator. Proper if nil or NULL pair; anything else improper. */
  if(w->pair_cur.type == VM_TYPE_NIL) {
    return 0;
  }
  if(w->pair_cur.type == VM_TYPE_PAIR && w->pair_cur.value.pair == NULL) {
    return 0;
  }
  return -1;
}

void
vm_list_walker_terminator(const vm_list_walker_t *w, vm_obj_t *out)
{
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
  return obj->type == VM_TYPE_PAIR && obj->value.pair != NULL;
}

int
vm_obj_is_null(const vm_obj_t *obj)
{
  return obj->type == VM_TYPE_NIL;
}

int
vm_obj_is_proper_list(const vm_obj_t *obj)
{
  vm_list_walker_t w;
  vm_obj_t *car;
  int status;

  if(obj->type == VM_TYPE_NIL) {
    return 1;
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

void
vm_pair_builder_init(vm_pair_builder_t *b)
{
  b->head = NULL;
  b->tail = NULL;
}

int
vm_pair_builder_append(vm_pair_builder_t *b, const vm_obj_t *obj)
{
  vm_pair_t *p;

  p = vm_alloc(sizeof(vm_pair_t));
  if(p == NULL) {
    return 0;
  }
  p->car = *obj;
  /* New pair becomes the new tail; its cdr is the nil singleton
     until another append extends the chain. */
  p->cdr.type = VM_TYPE_NIL;

  if(b->head == NULL) {
    b->head = p;
  } else {
    b->tail->cdr.type = VM_TYPE_PAIR;
    b->tail->cdr.value.pair = p;
  }
  b->tail = p;
  return 1;
}

void
vm_pair_builder_result(const vm_pair_builder_t *b, vm_obj_t *out)
{
  if(b->head == NULL) {
    /* Empty result: emit the R7RS nil singleton, not a vm_list_t of
       length 0. The nil tag is the value; no allocation needed. */
    out->type = VM_TYPE_NIL;
    return;
  }
  out->type = VM_TYPE_PAIR;
  out->value.pair = b->head;
}
