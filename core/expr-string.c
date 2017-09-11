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

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vm-functions.h"
#include "vm-list.h"

#define NUMBER_BITS (sizeof(long) * CHAR_BIT)
#define NUMBER_STRING_LENGTH NUMBER_BITS

static int
print_bits(char *str, size_t len, long number)
{
  uint8_t is_negative, leading_zeroes, bit;
  unsigned i, j;

  is_negative = number < 0;
  j = 0;
  if(is_negative) {
    number = -number;
    str[j++] = '-';
  }
  leading_zeroes = 1;

  for(i = 0; i < NUMBER_BITS; i++) {
    bit = (number >> (NUMBER_BITS - i - 1)) & 1;

    if(bit) {
      leading_zeroes = 0;
    }

    if(!leading_zeroes || i + 1 == NUMBER_BITS) {
      /* Print the bit if it is not a leading zero, or if it is
         the only zero. */
      str[j++] = '0' + bit;
    }
  }

  return j;
}

VM_FUNCTION(make_string)
{
  vm_string_t *string;

  if(argv[0].type != VM_TYPE_INTEGER ||
     (argc == 2 && argv[1].type != VM_TYPE_CHARACTER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  string = vm_string_create(&thread->result, argv[0].value.integer, NULL);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  memset(string->str, argc == 2 ? argv[1].value.character : 0, string->length);
  string->str[string->length] = '\0';
}

VM_FUNCTION(string)
{
  vm_string_t *string;
  int i;

  string = vm_string_create(&thread->result, argc, NULL);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0; i < argc; i++) {
    if(argv[i].type != VM_TYPE_CHARACTER) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      vm_free(string->str);
      return;
    }
    string->str[i] = argv[i].value.character;
  }
  string->str[string->length] = '\0';
}

VM_FUNCTION(stringp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_STRING);
}

VM_FUNCTION(string_length)
{
  VM_PUSH_INTEGER(argv[0].value.string->length);
}

VM_FUNCTION(string_ref)
{
  vm_string_t *string;
  vm_integer_t k;

  if(argv[0].type != VM_TYPE_STRING || argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  string = argv[0].value.string;
  k = argv[1].value.integer;

  if(k < 0 || k >= string->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
  } else {
    VM_PUSH_CHARACTER(string->str[k]);
  }
}

VM_FUNCTION(string_set)
{
  vm_string_t *string;
  vm_integer_t k;

  if(argv[0].type != VM_TYPE_STRING ||
     argv[1].type != VM_TYPE_INTEGER ||
     argv[2].type != VM_TYPE_CHARACTER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  string = argv[0].value.string;
  k = argv[1].value.integer;

  if(IS_SET(string->flags, VM_STRING_FLAG_IMMUTABLE)) {
    vm_signal_error(thread, VM_ERROR_WRITE_PROHIBITED);
  } else if(k < 0 || k >= string->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
  } else {
    string->str[k] = argv[1].value.character;
  }
}

VM_FUNCTION(string_to_list)
{
  vm_string_t *string;
  vm_list_t *list;
  vm_integer_t i;
  vm_obj_t obj;

  string = argv[0].value.string;

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  obj.type = VM_TYPE_CHARACTER;
  for(i = 0; i < string->length; i++) {
    obj.value.character = string->str[i];
    if(!vm_list_insert_tail(list, &obj)) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  VM_PUSH_LIST(list);
}

VM_FUNCTION(list_to_string)
{
  vm_list_t *list;
  vm_string_t *string;
  vm_integer_t i;
  vm_list_item_t *item;

  list = argv[0].value.list;
  string = vm_string_create(&thread->result, list->length, NULL);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0, item = list->head; item != NULL; i++, item = item->next) {
    if(item->obj.type != VM_TYPE_CHARACTER) {
      vm_free(string->str);
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    string->str[i] = item->obj.value.character;
  }
}

VM_FUNCTION(vector_to_string)
{
  vm_vector_t *vector;
  vm_string_t *string;
  vm_integer_t i;

  vector = argv[0].value.vector;
  if(IS_CLEAR(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  string = vm_string_create(&thread->result, vector->length, NULL);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0; i < vector->length; i++) {
    string->str[i] = vector->bytes[i];
  }
}

VM_FUNCTION(string_fill)
{
  vm_string_t *string;

  if(argv[0].type != VM_TYPE_STRING ||
     argv[1].type != VM_TYPE_CHARACTER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  string = argv[0].value.string;
  if(IS_SET(string->flags, VM_STRING_FLAG_IMMUTABLE)) {
    vm_signal_error(thread, VM_ERROR_WRITE_PROHIBITED);
  } else {
    memset(string->str, argv[1].value.character, string->length);
  }
}

VM_FUNCTION(string_compare)
{
  char *str1;
  char *str2;

  str1 = argv[0].value.string->str;
  str2 = argv[1].value.string->str;

  if(str1 != NULL && str2 != NULL) {
    VM_PUSH_INTEGER(strcmp(str1, str2));
  }
}

VM_FUNCTION(substring)
{
  vm_string_t *substring;
  vm_string_t *string;
  vm_integer_t start;
  vm_integer_t end;

  if(argv[0].type != VM_TYPE_STRING  ||
     argv[1].type != VM_TYPE_INTEGER ||
     argv[2].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  string = argv[0].value.string;
  start = argv[1].value.integer;
  end = argv[2].value.integer;

  if(start < 0 || end < start || end > string->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    if(start < 0) {
      vm_set_error_object(thread, &argv[1]);
    } else {
      vm_set_error_object(thread, &argv[2]);
    }
    return;
  }

  substring = vm_string_create(&thread->result, end - start, NULL);
  if(substring == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  strncpy(substring->str, string->str + start, substring->length);
  substring->str[substring->length] = '\0';
}

VM_FUNCTION(string_append)
{
  vm_string_t *result;
  vm_string_t *string;
  vm_integer_t result_length;
  int i;
  int offset;

  for(i = result_length = 0; i < argc; i++) {
    string = argv[i].value.string;
    result_length += string->length;
  }

  result = vm_string_create(&thread->result, result_length, NULL);
  if(result == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0, offset = 0; i < argc; i++) {
    string = argv[i].value.string;
    memcpy(result->str + offset, string->str, string->length);
    offset += string->length;
  }

  result->str[result->length] = '\0';

  thread->result.type = VM_TYPE_STRING;
}

VM_FUNCTION(string_copy)
{
  vm_string_t *copy, *original;

  original = argv[0].value.string;

  copy = vm_string_create(&thread->result, original->length, original->str);
  if(copy == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
}

VM_FUNCTION(string_split)
{
  char *string;
  char *seps;
  char *save_ptr;
  char *substring;
  vm_list_t *list;
  vm_obj_t obj;

  string = argv[0].value.string->str;
  seps = argv[1].value.string->str;

  if(string == NULL || seps == NULL) {
    return;
  }

  string = VM_STRDUP(string);
  if(string == NULL) {
    return;
  }

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    VM_FREE(string);
    return;
  }

  obj.type = VM_TYPE_STRING;

  save_ptr = string;
  do {
    substring = strtok_r(string, seps, &save_ptr);
    string = NULL;
    if(substring == NULL) {
      break;
    }

    if(vm_string_create(&obj, -1, substring) == NULL ||
       !vm_list_insert_tail(list, &obj)) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      VM_FREE(string);
      return;
    }
  } while(substring != NULL);

  VM_PUSH_LIST(list);
}

VM_FUNCTION(number_to_string)
{
  uint8_t radix;
  long number;
  const char *format;
  vm_string_t *string;

  if(argv[0].type != VM_TYPE_INTEGER ||
     (argc == 2 && argv[1].type != VM_TYPE_INTEGER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  radix = argc == 2 ? argv[1].value.integer : 10;
  switch(radix) {
  case 2:
    format = NULL;
    break;
  case 8:
    format = "%o";
    break;
  case 10:
    format = "%ld";
    break;
  case 16:
    format = "0x%x";
    break;
    break;
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
    return;
  }

  number = (long)argv[0].value.integer;

  string = vm_string_create(&thread->result, NUMBER_STRING_LENGTH, NULL);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  if(format == NULL) {
    string->length = print_bits(string->str, NUMBER_STRING_LENGTH, number);
  } else {
    string->length = snprintf(string->str, NUMBER_STRING_LENGTH,
                              format, number);
  }

  if(string->length < 0) {
    vm_free(string->str);
    vm_signal_error(thread, VM_ERROR_INTERNAL);
    return;
  }
}

VM_FUNCTION(string_to_number)
{
  uint8_t radix;

  if(argv[0].type != VM_TYPE_STRING ||
     (argc == 2 && argv[1].type != VM_TYPE_INTEGER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  radix = argc == 2 ? argv[1].value.integer : 10;
  VM_PUSH_INTEGER(strtol(argv[0].value.string->str, NULL, radix));
}
