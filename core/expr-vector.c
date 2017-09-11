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
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
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
#include "vm-list.h"

VM_FUNCTION(make_vector)
{
  vm_vector_t *vector;
  vm_integer_t i;

  if(argv[0].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vector = vm_vector_create(&thread->result, argv[0].value.integer,
                            VM_VECTOR_FLAG_REGULAR);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  if(argc == 2) {
    for(i = 0; i < vector->length; i++) {
      memcpy(&vector->elements[i], &argv[1], sizeof(vm_obj_t));
    }
  } else {
    for(i = 0; i < vector->length; i++) {
      vector->elements[i].type = VM_TYPE_NONE;
    }
  }
}

VM_FUNCTION(vector)
{
  vm_vector_t *vector;

  vector = vm_vector_create(&thread->result, argc, VM_VECTOR_FLAG_REGULAR);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  memcpy(vector->elements, argv, sizeof(vm_obj_t) * argc);
}

VM_FUNCTION(vectorp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_VECTOR);
}

VM_FUNCTION(bufferp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_VECTOR &&
                  IS_SET(argv[0].value.vector->flags, VM_VECTOR_FLAG_BUFFER));
}

VM_FUNCTION(vector_merge)
{
  vm_vector_t *vector;
  int i;
  int j;
  int k;
  unsigned element_count;

  element_count = 0;
  for(i = 0; i < argc; i++) {
    element_count += argv[i].type == VM_TYPE_VECTOR ?
                       argv[i].value.vector->length : 1;
  }

  vector = vm_vector_create(&thread->result, element_count,
                            VM_VECTOR_FLAG_REGULAR);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0, j = 0; i < argc; i++) {
    if(argv[i].type == VM_TYPE_VECTOR) {
      if(IS_SET(argv[i].value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
        for(k = 0; k < argv[i].value.vector->length; k++) {
          vector->elements[j + k].type = VM_TYPE_INTEGER;
          vector->elements[j + k].value.integer = argv[i].value.vector->bytes[k];
        }
      } else {
        memcpy(&vector->elements[j], argv[i].value.vector->elements,
               sizeof(vm_obj_t) * argv[i].value.vector->length);
      }
      j += argv[i].value.vector->length;
    } else {
      memcpy(&vector->elements[j], &argv[i], sizeof(vm_obj_t));
      j++;
    }
  }
}

VM_FUNCTION(vector_length)
{
  VM_PUSH_INTEGER(argv[0].value.vector->length);
}

VM_FUNCTION(vector_ref)
{
  vm_vector_t *vector;
  vm_integer_t k;

  if(argv[0].type != VM_TYPE_VECTOR || argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vector = argv[0].value.vector;
  k = argv[1].value.integer;
  if(k < 0 || k >= vector->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
  } else {
    if(IS_SET(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
      VM_PUSH_CHARACTER(vector->bytes[k]);
    } else {
      VM_PUSH(&vector->elements[k]);
    }
  }
}

VM_FUNCTION(vector_set)
{
  vm_integer_t k;
  int r;

  if(argv[0].type != VM_TYPE_VECTOR || argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  k = argv[1].value.integer;
  r = vm_vector_set(&argv[0], k, &argv[2]);
  if(r < 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    if(r == -1) {
      vm_set_error_object(thread, &argv[1]);
    } else {
      vm_set_error_string(thread, "buffer element must be integer or character type");
    }
  }
}

VM_FUNCTION(vector_to_list)
{
  vm_vector_t *vector;
  vm_list_t *list;
  vm_integer_t i;
  vm_obj_t ch;

  vector = argv[0].value.vector;

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  if(IS_SET(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    ch.type = VM_TYPE_CHARACTER;
    for(i = 0; i < vector->length; i++) {
      ch.value.character = vector->bytes[i];
      if(!vm_list_insert_tail(list, &ch)) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }
  } else {
    for(i = 0; i < vector->length; i++) {
      if(!vm_list_insert_tail(list, vector->elements + i)) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }
  }

  VM_PUSH_LIST(list);
}

VM_FUNCTION(list_to_vector)
{
  vm_list_t *list;
  vm_integer_t i;
  vm_list_item_t *item;

  list = argv[0].value.list;
  if(vm_vector_create(&thread->result, list->length,
                      VM_VECTOR_FLAG_REGULAR) == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  } else {
    for(i = 0, item = list->head; item != NULL; i++, item = item->next) {
      if(vm_vector_set(&thread->result, i, &item->obj) < 0) {
	vm_signal_error(thread, VM_ERROR_INTERNAL);
	return;
      }
    }
  }
}

VM_FUNCTION(vector_fill)
{
  vm_vector_t *vector;
  vm_integer_t i;
  int r;

  if(argv[0].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vector = argv[0].value.vector;

  for(i = 0; i < vector->length; i++) {
    r = vm_vector_set(&argv[0], i, &argv[1]);
    if(r < 0) {
      vm_signal_error(thread,
                      r == -1 ? VM_ERROR_INTERNAL : VM_ERROR_ARGUMENT_VALUE);
      return;
    }
  }
}

VM_FUNCTION(make_buffer)
{
  vm_vector_t *vector;

  if(argv[0].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vector = vm_vector_create(&thread->result, argv[0].value.integer,
                            VM_VECTOR_FLAG_BUFFER);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
}

VM_FUNCTION(buffer_append)
{
  vm_vector_t *dst_vector;
  vm_vector_t *vector;
  vm_string_t *string;
  vm_integer_t index;
  unsigned byte_count;
  uint8_t byte_value;

  if(argv[0].type != VM_TYPE_VECTOR || argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  dst_vector = argv[0].value.vector;
  if(IS_CLEAR(dst_vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  index = argv[1].value.integer;

  /* Check whether there is room for one more byte in the buffer. */
  byte_count = 1;
  if(index < 0 || index + byte_count > (unsigned)dst_vector->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }

  switch(argv[2].type) {
  case VM_TYPE_CHARACTER:
    byte_value = (unsigned)argv[2].value.character & 0xff;
    dst_vector->bytes[index] = byte_value;
    break;
  case VM_TYPE_INTEGER:
    byte_value = (unsigned)argv[2].value.integer & 0xff;
    dst_vector->bytes[index] = byte_value;
    break;
  case VM_TYPE_VECTOR:
    vector = argv[2].value.vector;
    if(IS_CLEAR(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }
    byte_count = vector->length;

    /* Check buffer boundary. */
    if(index + byte_count > (unsigned)dst_vector->length) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }

    memcpy(&dst_vector->bytes[index], vector->bytes, byte_count);

    break;
  case VM_TYPE_STRING:
    string = argv[2].value.string;
    byte_count = string->length;

    /* Check buffer boundary. */
    if(index + byte_count > (unsigned)dst_vector->length) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }

    memcpy(&dst_vector->bytes[index], string->str, byte_count);
    break;
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  VM_PUSH_INTEGER(byte_count);
}
