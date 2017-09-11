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

#include "vm.h"
#include "vm-bytecode.h"
#include "vm-log.h"

/* Instruction pointer management macros. */
#define VM_IP(thread) (*(thread)->expr->ip)
#define VM_CHECK_BOUNDARY(thread, n) \
  ((thread)->expr->ip + (n) <= (thread)->expr->end)
#define VM_STEP_N(thread, n) \
  (VM_CHECK_BOUNDARY(thread, (n)) ? \
    ((thread)->expr->ip += (n)) : \
    (vm_signal_error((thread), VM_ERROR_BYTECODE), NULL))
#define VM_STEP(thread) VM_STEP_N((thread), 1)

/* Instruction definition retrieval macros. */
#define VM_GET_TYPE(thread)        (VM_IP(thread) & VM_ATOM_MASK)
#define VM_GET_TOKEN(thread)       (VM_IP(thread) >> 7)
#define VM_GET_FORM_TYPE(thread)   ((VM_IP(thread) >> 5) & 3)
#define VM_GET_EMBEDDED(thread)    ((VM_IP(thread) >> 3) & 15)

/* Value definition retrieval macros. */
#define VM_GET_BOOLEAN(thread)      (VM_GET_EMBEDDED(thread) & 1)
#define VM_GET_INTEGER_SIZE(thread) ((unsigned)(VM_GET_EMBEDDED(thread) & 7))
#define VM_GET_INTEGER_SIGN(thread) ((unsigned)(VM_GET_EMBEDDED(thread) >> 3))
#define VM_GET_ARGC(thread)         ((unsigned)(VM_IP(thread) & 63))

static vm_integer_t
get_integer(vm_thread_t *thread)
{
  vm_integer_t nbytes;
  vm_boolean_t is_negative;
  vm_integer_t integer;
  vm_integer_t i;

  nbytes = VM_GET_INTEGER_SIZE(thread);

  if(nbytes < 1 || nbytes > 4) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Invalid integer size: %d",
             (int)nbytes);
    vm_signal_error(thread, VM_ERROR_BYTECODE);
    return 0;
  }

  is_negative = VM_GET_INTEGER_SIGN(thread);
  if(!VM_STEP(thread) || !VM_CHECK_BOUNDARY(thread, nbytes)) {
    return 0;
  }

  /* TODO: Rewrite into something faster. */
  for(integer = i = 0; i < nbytes; i++) {
    integer |= (int32_t)thread->expr->ip[i] << (8 * (nbytes - i - 1));
  }

  thread->expr->ip += nbytes;

  return is_negative ? -integer : integer;
}

static vm_expr_id_t
get_expr_id(vm_thread_t *thread)
{
  vm_expr_id_t expr_id;

  expr_id = VM_IP(thread) & 0xf;

   if(!(VM_IP(thread) & 0x10)) {
    /* Extended form id. */
    if(!VM_STEP(thread)) {
      return 0;
    }
    expr_id <<= 8;
    expr_id |= VM_IP(thread);
  }

  if(!VM_STEP(thread)) {
    return 0;
  }

  if(expr_id >= VM_TABLE_SIZE(thread->program->exprv)) {
    vm_signal_error(thread, VM_ERROR_EXPR_ID);
  }

  return expr_id;
}

void
get_symbol_ref(vm_thread_t *thread, vm_symbol_ref_t *symbol_ref)
{
  uint8_t byte;

  if(!VM_STEP(thread)) {
    return;
  }

  byte = VM_IP(thread);
  symbol_ref->symbol_id = byte & 0x3f;

  if(!VM_STEP(thread)) {
    return;
  }

  symbol_ref->scope = (byte >> 7) & 0x1;

  if(byte & 0x40) {
    byte = VM_IP(thread);
    symbol_ref->symbol_id <<= 8;
    symbol_ref->symbol_id |= byte;
    if(!VM_STEP(thread)) {
      return;
    }
  }
}

void
vm_get_object(vm_thread_t *thread, vm_obj_t *obj)
{
  if(VM_GET_TOKEN(thread) == VM_TOKEN_FORM) {
    obj->type = VM_TYPE_FORM;
    obj->value.form.type = VM_GET_FORM_TYPE(thread);

    switch(obj->value.form.type) {
    case VM_FORM_INLINE:
      obj->value.form.id = 0;
      obj->value.form.argc = VM_GET_ARGC(thread);
      if(!VM_STEP(thread)) {
        return;
      }
      break;
    case VM_FORM_LAMBDA:
    case VM_FORM_REF:
      obj->value.form.id = get_expr_id(thread);
      obj->value.form.argc = 0;
      break;
    default:
      vm_signal_error(thread, VM_ERROR_BYTECODE);
    }

    return;
  }

  obj->type = VM_GET_TYPE(thread);

  switch(obj->type) {
  case VM_TYPE_BOOLEAN:
    obj->value.boolean = VM_GET_BOOLEAN(thread);
    if(!VM_STEP(thread)) {
      return;
    }
    break;
  case VM_TYPE_INTEGER:
    obj->value.integer = get_integer(thread);
    break;
  case VM_TYPE_RATIONAL:
    if(!VM_STEP(thread)) {
      return;
    }
    obj->value.rational = vm_alloc(sizeof(vm_rational_t));
    if(obj->value.rational == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    obj->value.rational->numerator = get_integer(thread);
    obj->value.rational->denominator = get_integer(thread);
    if(obj->value.rational->denominator == 0) {
      vm_signal_error(thread, VM_ERROR_BYTECODE);
      return;
    }
    break;
  case VM_TYPE_STRING:
    if(!VM_STEP(thread)) {
      return;
    }
    obj->value.string = vm_alloc(sizeof(vm_string_t));
    if(obj->value.string == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    obj->value.string->flags = VM_STRING_FLAG_ID | VM_STRING_FLAG_IMMUTABLE;
    obj->value.string->string_id = VM_IP(thread);
    if(!VM_STEP(thread)) {
      return;
    }
    break;
  case VM_TYPE_CHARACTER:
    if(!VM_STEP(thread)) {
      return;
    }
    obj->value.character = VM_IP(thread);
    if(!VM_STEP(thread)) {
      return;
    }
    break;
  case VM_TYPE_SYMBOL:
    get_symbol_ref(thread, &obj->value.symbol_ref);
    break;
  default:
    vm_signal_error(thread, VM_ERROR_BYTECODE);
    break;
  }
}
