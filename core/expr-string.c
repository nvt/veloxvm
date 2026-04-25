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
  vm_string_t *string = argv[0].value.string;
  if(vm_string_resolve(thread, string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }
  VM_PUSH_INTEGER(string->length);
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
  if(vm_string_resolve(thread, string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }
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
  if(vm_string_resolve(thread, string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }
  k = argv[1].value.integer;

  if(VM_IS_SET(string->flags, VM_STRING_FLAG_IMMUTABLE)) {
    vm_signal_error(thread, VM_ERROR_WRITE_PROHIBITED);
  } else if(k < 0 || k >= string->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[1]);
  } else {
    string->str[k] = argv[2].value.character;
  }
}

VM_FUNCTION(string_to_list)
{
  vm_string_t *string;
  vm_list_t *list;
  vm_integer_t i;
  vm_obj_t obj;

  string = argv[0].value.string;
  if(vm_string_resolve(thread, string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }

  /* Disable GC during list construction */
  vm_gc_disable();

  list = vm_list_create();
  if(list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  obj.type = VM_TYPE_CHARACTER;
  for(i = 0; i < string->length; i++) {
    obj.value.character = string->str[i];
    if(!vm_list_insert_tail(list, &obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  vm_gc_enable();
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
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    string->str[i] = item->obj.value.character;
  }
  string->str[string->length] = '\0';
}

VM_FUNCTION(vector_to_string)
{
  vm_vector_t *vector;
  vm_string_t *string;
  vm_integer_t i;

  vector = argv[0].value.vector;

  /* Create string with same length as vector */
  string = vm_string_create(&thread->result, vector->length, NULL);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  /* Handle buffer vectors (byte arrays) */
  if(VM_IS_SET(vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    for(i = 0; i < vector->length; i++) {
      string->str[i] = vector->bytes[i];
    }
  } else {
    /* Handle regular vectors - must contain only characters */
    for(i = 0; i < vector->length; i++) {
      if(vector->elements[i].type != VM_TYPE_CHARACTER) {
        vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
        return;
      }
      string->str[i] = vector->elements[i].value.character;
    }
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
  if(vm_string_resolve(thread, string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }
  if(VM_IS_SET(string->flags, VM_STRING_FLAG_IMMUTABLE)) {
    vm_signal_error(thread, VM_ERROR_WRITE_PROHIBITED);
  } else {
    memset(string->str, argv[1].value.character, string->length);
  }
}

VM_FUNCTION(string_compare)
{
  char *str1;
  char *str2;

  if(vm_string_resolve(thread, argv[0].value.string) == NULL ||
     vm_string_resolve(thread, argv[1].value.string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }

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
  if(vm_string_resolve(thread, string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }
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

  /* Strings stored by ID (e.g. literals bound to top-level names) have
     length 0 and a NULL str pointer until vm_string_resolve runs --
     skipping the resolve here silently dropped those args. */
  for(i = result_length = 0; i < argc; i++) {
    string = argv[i].value.string;
    if(vm_string_resolve(thread, string) == NULL) {
      vm_signal_error(thread, VM_ERROR_STRING_ID);
      return;
    }
    if(string->length > VM_STRING_MAX_LENGTH - result_length) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }
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
  char *resolved_str;

  original = argv[0].value.string;

  /* Ensure the string is resolved before copying */
  resolved_str = vm_string_resolve(thread, original);
  if(resolved_str == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }

  copy = vm_string_create(&thread->result, original->length, resolved_str);
  if(copy == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
}

VM_FUNCTION(string_split)
{
  const char *string;
  const char *seps;
  const char *p;
  vm_list_t *list;
  vm_obj_t obj;

  if(vm_string_resolve(thread, argv[0].value.string) == NULL ||
     vm_string_resolve(thread, argv[1].value.string) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }

  string = argv[0].value.string->str;
  seps = argv[1].value.string->str;

  if(string == NULL || seps == NULL) {
    return;
  }

  /* Disable GC during list construction */
  vm_gc_disable();

  list = vm_list_create();
  if(list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  obj.type = VM_TYPE_STRING;

  p = string;
  while(*p != '\0') {
    size_t tok_len;

    p += strspn(p, seps);
    if(*p == '\0') {
      break;
    }

    tok_len = strcspn(p, seps);

    if(vm_string_create(&obj, tok_len, p) == NULL ||
       !vm_list_insert_tail(list, &obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    p += tok_len;
  }

  vm_gc_enable();
  VM_PUSH_LIST(list);
}

VM_FUNCTION(string_join)
{
  vm_string_t *separator;
  vm_list_t *list;
  vm_list_item_t *item;
  vm_string_t *result;
  vm_integer_t total_length;
  vm_integer_t num_separators;
  int offset;

  /* Validate argument types */
  if(argv[0].type != VM_TYPE_STRING || argv[1].type != VM_TYPE_LIST) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  separator = argv[0].value.string;
  if(vm_string_resolve(thread, separator) == NULL) {
    vm_signal_error(thread, VM_ERROR_STRING_ID);
    return;
  }
  list = argv[1].value.list;

  /* Empty list returns empty string */
  if(list->length == 0) {
    result = vm_string_create(&thread->result, 0, "");
    if(result == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
    }
    return;
  }

  /* Calculate total length needed */
  total_length = 0;
  num_separators = list->length - 1;

  for(item = list->head; item != NULL; item = item->next) {
    if(item->obj.type != VM_TYPE_STRING) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    if(vm_string_resolve(thread, item->obj.value.string) == NULL) {
      vm_signal_error(thread, VM_ERROR_STRING_ID);
      return;
    }
    if(item->obj.value.string->length > VM_STRING_MAX_LENGTH - total_length) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      return;
    }
    total_length += item->obj.value.string->length;
  }

  /* Add separator lengths with overflow check. */
  if(separator->length != 0 &&
     num_separators > (VM_STRING_MAX_LENGTH - total_length) / separator->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  total_length += num_separators * separator->length;

  /* Create result string */
  result = vm_string_create(&thread->result, total_length, NULL);
  if(result == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  /* Join strings with separator */
  offset = 0;
  for(item = list->head; item != NULL; item = item->next) {
    vm_string_t *str = item->obj.value.string;

    /* Copy the string */
    memcpy(result->str + offset, str->str, str->length);
    offset += str->length;

    /* Add separator if not the last item */
    if(item->next != NULL) {
      memcpy(result->str + offset, separator->str, separator->length);
      offset += separator->length;
    }
  }

  result->str[result->length] = '\0';
  thread->result.type = VM_TYPE_STRING;
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
    format = "%x";
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
  char *endptr;
  long result;
  char *str;

  if(argv[0].type != VM_TYPE_STRING ||
     (argc == 2 && argv[1].type != VM_TYPE_INTEGER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  radix = argc == 2 ? argv[1].value.integer : 10;
  str = argv[0].value.string->str;

  /* Skip leading whitespace */
  while(*str == ' ' || *str == '\t' || *str == '\n' || *str == '\r') {
    str++;
  }

  /* Empty string or only whitespace returns #f */
  if(*str == '\0') {
    VM_PUSH_BOOLEAN(0);
    return;
  }

  result = strtol(str, &endptr, radix);

  /* Check if conversion was successful - endptr should not equal str,
     and we should have consumed the entire string (except trailing whitespace) */
  if(endptr == str) {
    /* No conversion performed */
    VM_PUSH_BOOLEAN(0);
    return;
  }

  /* Skip trailing whitespace */
  while(*endptr == ' ' || *endptr == '\t' || *endptr == '\n' || *endptr == '\r') {
    endptr++;
  }

  /* If we haven't consumed the entire string, it's invalid */
  if(*endptr != '\0') {
    VM_PUSH_BOOLEAN(0);
    return;
  }

  VM_PUSH_INTEGER(result);
}

VM_FUNCTION(symbol_to_string)
{
  const char *symbol_name;
  vm_string_t *string;
  size_t length;

  if(argv[0].type != VM_TYPE_SYMBOL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  symbol_name = vm_symbol_lookup(thread->program, &argv[0].value.symbol_ref);
  if(symbol_name == NULL) {
    vm_signal_error(thread, VM_ERROR_SYMBOL_UNDEFINED);
    return;
  }

  length = strlen(symbol_name);
  string = vm_string_create(&thread->result, length, symbol_name);
  if(string == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
}
