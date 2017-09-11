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

  obj->type = VM_TYPE_STRING;
  obj->value.string = vm_alloc(sizeof(vm_string_t));
  if(obj->value.string == NULL) {
    return NULL;
  }
  string = obj->value.string;
  string->flags = VM_STRING_FLAG_RESOLVED;
  final_length = length < 0 ? strlen(str) : (size_t)length;
  if(final_length > VM_STRING_MAX_LENGTH) {
    return NULL;
  }
  string->length = final_length;
  string->str = vm_alloc(final_length + 1);
  if(string->str == NULL) {
    return NULL;
  }

  if(str != NULL) {
    strncpy(string->str, str, final_length);
    string->str[final_length] = '\0';
  } else {
    string->str[0] = '\0';
  }

  return string;
}

char *
vm_string_resolve(vm_thread_t *thread, vm_string_t *string)
{
  if(IS_SET(string->flags, VM_STRING_FLAG_ID) &&
     IS_CLEAR(string->flags, VM_STRING_FLAG_RESOLVED)) {
    SET(string->flags, VM_STRING_FLAG_RESOLVED | VM_STRING_FLAG_IMMUTABLE);

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

  if(length <= 0) {
    return 0;
  }

  obj->type = VM_TYPE_VECTOR;
  obj->value.vector = vm_alloc(sizeof(vm_vector_t));
  if(obj->value.vector == NULL) {
    return NULL;
  }
  vector = obj->value.vector;
  vector->flags = flags;
  vector->length = length;

  if(IS_SET(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vector->bytes = vm_alloc(sizeof(uint8_t) * vector->length);
    if(vector->bytes == NULL) {
      return NULL;
    }
    memset(vector->bytes, 0, vector->length);
    vector->elements = NULL;
  } else {
    vector->elements = vm_alloc(sizeof(vm_obj_t) * vector->length);
    if(vector->elements == NULL) {
      return NULL;
    }
    vector->bytes = NULL;
  }

  return vector;
}

int
vm_vector_set(vm_obj_t *vector, vm_integer_t index, vm_obj_t *obj)
{
  if(index < 0 || index >= vector->value.vector->length) {
    return -1;
  }

  if(IS_SET(vector->value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
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
