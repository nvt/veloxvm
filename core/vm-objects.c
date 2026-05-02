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

static vm_ext_type_id_t type_id_counter;

static const char *
string_lookup(vm_program_t *program, vm_string_id_t string_id)
{
  return string_id >= VM_TABLE_SIZE(program->strings) ?
         NULL : (const char *)VM_TABLE_GET(program->strings, string_id);
}

void
vm_ext_type_register(vm_ext_type_t *ext_type)
{
  ext_type->type_id = ++type_id_counter;
}

vm_string_t *
vm_string_create(vm_obj_t *obj, vm_integer_t length, const char *str)
{
  vm_string_t *string;
  size_t final_length;

  if(length < 0 && str == NULL) {
    return NULL;
  }

  /* Disable GC across the multi-step setup so a sweep from inside the
     second vm_alloc cannot observe the string with RESOLVED set but
     ->str still uninitialized. */
  vm_gc_disable();

  obj->type = VM_TYPE_STRING;
  obj->value.string = vm_alloc_at(sizeof(vm_string_t),
                                  VM_ALLOC_SITE_STRING_HEADER);
  if(obj->value.string == NULL) {
    vm_gc_enable();
    return NULL;
  }
  string = obj->value.string;
  string->flags = VM_STRING_FLAG_RESOLVED;
  string->str = NULL;
  final_length = length < 0 ? strlen(str) : (size_t)length;
  if(final_length > VM_STRING_MAX_LENGTH) {
    vm_gc_enable();
    return NULL;
  }
  string->length = final_length;
  string->str = vm_alloc_at(final_length + 1, VM_ALLOC_SITE_STRING_BUFFER);
  if(string->str == NULL) {
    vm_gc_enable();
    return NULL;
  }

  if(str != NULL) {
    strncpy(string->str, str, final_length);
    string->str[final_length] = '\0';
  } else {
    string->str[0] = '\0';
  }

  vm_gc_enable();
  return string;
}

char *
vm_string_resolve(vm_thread_t *thread, vm_string_t *string)
{
  if(VM_IS_SET(string->flags, VM_STRING_FLAG_ID) &&
     VM_IS_CLEAR(string->flags, VM_STRING_FLAG_RESOLVED)) {
    VM_SET_FLAG(string->flags, VM_STRING_FLAG_RESOLVED | VM_STRING_FLAG_IMMUTABLE);

    if(thread == NULL) {
      thread = vm_current_thread();
    }
    if(thread != NULL) {
      string->str = (char *)string_lookup(thread->program,
                                          string->string_id);
    }
    if(string->str != NULL) {
      string->length = strlen(string->str);
    } else {
      vm_signal_error(thread, VM_ERROR_STRING_ID);
    }
  }
  return string->str;
}

vm_vector_t *
vm_vector_create(vm_obj_t *obj, vm_integer_t length, vm_vector_flags_t flags)
{
  vm_vector_t *vector;

  if(length < 0) {
    return 0;
  }

  /* Disable GC across the multi-step setup so a sweep from inside the
     second vm_alloc cannot observe the vector header with garbage
     ->bytes / ->elements pointers. */
  vm_gc_disable();

  obj->type = VM_TYPE_VECTOR;
  obj->value.vector = vm_alloc_at(sizeof(vm_vector_t),
                                  VM_ALLOC_SITE_VECTOR_HEADER);
  if(obj->value.vector == NULL) {
    vm_gc_enable();
    return NULL;
  }
  vector = obj->value.vector;
  vector->flags = flags;
  vector->length = length;
  vector->elements = NULL;
  vector->bytes = NULL;

  if(length == 0) {
    /* Zero-length vector: no elements/bytes to allocate */
  } else if(VM_IS_SET(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vector->bytes = vm_alloc_at(sizeof(uint8_t) * vector->length,
                                VM_ALLOC_SITE_VECTOR_BYTES);
    if(vector->bytes == NULL) {
      vm_gc_enable();
      return NULL;
    }
    memset(vector->bytes, 0, vector->length);
  } else {
    vector->elements = vm_alloc_at(sizeof(vm_obj_t) * vector->length,
                                   VM_ALLOC_SITE_VECTOR_ELEMENTS);
    if(vector->elements == NULL) {
      vm_gc_enable();
      return NULL;
    }
    /* Zero so the GC sees well-formed (boolean #f) cells if it walks
       the vector before the caller has populated the slots. */
    memset(vector->elements, 0, sizeof(vm_obj_t) * vector->length);
  }

  vm_gc_enable();
  return vector;
}

int
vm_vector_set(vm_obj_t *vector, vm_integer_t index, vm_obj_t *obj)
{
  if(index < 0 || index >= vector->value.vector->length) {
    return -1;
  }

  if(VM_IS_SET(vector->value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    switch(obj->type) {
    case VM_TYPE_INTEGER:
      vector->value.vector->bytes[index] = obj->value.integer & 0xff;
      break;
    case VM_TYPE_CHARACTER:
      vector->value.vector->bytes[index] = obj->value.character & 0xff;
      break;
    default:
      return -2;
    }
  } else {
    memcpy(&vector->value.vector->elements[index], obj, sizeof(vm_obj_t));
  }

  return 1;
}

vm_box_t *
vm_box_create(vm_obj_t *obj, const vm_obj_t *value)
{
  vm_box_t *box;

  obj->type = VM_TYPE_BOX;
  obj->value.box = vm_alloc(sizeof(vm_box_t));
  if(obj->value.box == NULL) {
    return NULL;
  }
  box = obj->value.box;
  if(value != NULL) {
    memcpy(&box->value, value, sizeof(vm_obj_t));
  } else {
    box->value.type = VM_TYPE_NONE;
  }
  return box;
}

vm_closure_t *
vm_closure_create(vm_obj_t *obj, vm_expr_id_t form_id, uint8_t argc,
                  uint8_t capture_count)
{
  vm_closure_t *closure;
  uint8_t i;

  /* Disable GC across the multi-step setup so a sweep from inside the
     captures alloc cannot observe the closure with a garbage ->captures
     pointer. */
  vm_gc_disable();

  obj->type = VM_TYPE_CLOSURE;
  obj->value.closure = vm_alloc(sizeof(vm_closure_t));
  if(obj->value.closure == NULL) {
    vm_gc_enable();
    return NULL;
  }
  closure = obj->value.closure;
  closure->form_id = form_id;
  closure->argc = argc;
  closure->capture_count = capture_count;
  closure->captures = NULL;
  if(capture_count > 0) {
    closure->captures = vm_alloc(sizeof(vm_obj_t) * capture_count);
    if(closure->captures == NULL) {
      vm_gc_enable();
      return NULL;
    }
    for(i = 0; i < capture_count; i++) {
      closure->captures[i].type = VM_TYPE_NONE;
    }
  }

  vm_gc_enable();
  return closure;
}
