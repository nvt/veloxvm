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

#ifndef VM_OBJECTS_H
#define VM_OBJECTS_H

#include <stdint.h>

/* VM object types. */
typedef enum vm_obj_type {
  VM_TYPE_BOOLEAN   =  0,
  VM_TYPE_INTEGER   =  1,
  VM_TYPE_RATIONAL  =  2,
  VM_TYPE_REAL      =  3,
  VM_TYPE_STRING    =  4,
  VM_TYPE_SYMBOL    =  5,
  VM_TYPE_CHARACTER =  6,
  VM_TYPE_FORM      =  7,
  VM_TYPE_LIST      =  8,
  VM_TYPE_VECTOR    =  9,
  VM_TYPE_PORT      = 10,
  VM_TYPE_COMPLEX   = 11,
  VM_TYPE_PROCEDURE = 12,
  VM_TYPE_EXTERNAL  = 13,
  VM_TYPE_NONE      = 14
} vm_obj_type_t;

/* Definitions for denoting multiple object types; e.g., in specifications
   of acceptable argument types for functions. */
typedef uint16_t vm_type_set_t;
#define VM_TYPE_FLAG(type)  ((vm_type_set_t)1 << (type))
#define VM_TYPE_FLAG_NUMBER (VM_TYPE_FLAG(VM_TYPE_INTEGER)  | \
                             VM_TYPE_FLAG(VM_TYPE_RATIONAL) | \
                             VM_TYPE_FLAG(VM_TYPE_REAL)     | \
                             VM_TYPE_FLAG(VM_TYPE_COMPLEX))
#define VM_TYPE_FLAG_ANY    (~((vm_type_set_t)0))

/* Value representations of VM object types. */

/* VM_TYPE_BOOLEAN representation. */
typedef enum vm_boolean {
  VM_FALSE = 0,
  VM_TRUE = 1
} vm_boolean_t;

/* VM_TYPE_INTEGER representation. */
typedef int32_t vm_integer_t;
#define VM_INTEGER_MAX INT32_MAX
#define VM_INTEGER_MIN INT32_MIN

/* VM_TYPE_RATIONAL representation. */
typedef struct vm_rational {
  vm_integer_t numerator;
  vm_integer_t denominator;
} vm_rational_t;

/* VM_TYPE_REAL representation. */
typedef double vm_real_t;

/* VM_TYPE_SYMBOL representation. */
typedef uint16_t vm_symbol_id_t;
typedef enum vm_symbol_scope {
  VM_SYMBOL_SCOPE_CORE = 0,
  VM_SYMBOL_SCOPE_APP = 1
} vm_symbol_scope_t;

typedef struct {
  vm_symbol_id_t symbol_id;
  vm_symbol_scope_t scope;
} vm_symbol_ref_t;

/* VM_TYPE_CHARACTER representation. */
typedef uint8_t vm_character_t;

/* VM_TYPE_STRING representation. */
#define VM_STRING_FLAG_ID 0x1
#define VM_STRING_FLAG_IMMUTABLE 0x2
#define VM_STRING_FLAG_RESOLVED 0x4

typedef uint16_t vm_string_id_t;

typedef struct vm_string {
  union {
    char *str;
    vm_string_id_t string_id;
  };
  int8_t length;
  uint8_t flags;
} vm_string_t;

#define VM_STRING_MAX_LENGTH INT8_MAX

/* VM_TYPE_PORT representation. */
#define VM_PORT_FLAG_INPUT 0x1
#define VM_PORT_FLAG_OUTPUT 0x2
#define VM_PORT_FLAG_EOF 0x4
#define VM_PORT_FLAG_OPEN 0x8
#define VM_PORT_FLAG_FILE 0x10
#define VM_PORT_FLAG_SOCKET 0x20
#define VM_PORT_FLAG_DEVICE 0x40
#define VM_PORT_FLAG_CONSOLE 0x80

struct vm_port_io;
struct vm_thread;
typedef struct vm_port {
  const struct vm_port_io *io;
  void *opaque_desc;
  struct vm_thread *thread;
  int fd;
  uint8_t flags;
} vm_port_t;

/* VM_TYPE_LIST representation. */
#define VM_LIST_FLAG_ORIGINAL 0x1
#define VM_LIST_FLAG_PAIR     0x2

struct vm_list_item;
typedef struct vm_list {
  struct vm_list_item *head;
  struct vm_list_item *tail;
  vm_integer_t length;
  uint8_t flags;
} vm_list_t;

/* VM_TYPE_VECTOR representation. */
typedef enum vm_vector_flags {
  VM_VECTOR_FLAG_REGULAR = 0x1,
  VM_VECTOR_FLAG_BUFFER = 0x2
} vm_vector_flags_t;

struct vm_obj;
typedef struct vm_vector {
  struct vm_obj *elements;
  uint8_t *bytes;
  vm_integer_t length;
  vm_vector_flags_t flags;
} vm_vector_t;

/* VM_TYPE_FORM representation. */
typedef uint16_t vm_expr_id_t;

typedef struct vm_form {
  vm_expr_id_t id;
  uint8_t type;
  uint8_t argc;
} vm_form_t;

typedef uint16_t vm_ext_type_id_t;

struct vm_ext_type;
typedef struct vm_ext_object {
  struct vm_ext_type *type;
  void *opaque_data;
} vm_ext_object_t;

/* Forward reference to a type definition needed by vm_obj_value_t. */
struct vm_procedure;

/* Inside VM objects, all value representations are stored within a union
   for space efficiency. */
typedef union vm_obj_value {
  vm_boolean_t boolean;
  vm_integer_t integer;
  vm_rational_t *rational;
#if VM_ENABLE_REALS
  vm_real_t real;
#endif
  vm_string_t *string;
  vm_symbol_ref_t symbol_ref;
  vm_form_t form;
  vm_character_t character;
  vm_list_t *list;
  vm_vector_t *vector;
  vm_port_t *port;
  const struct vm_procedure *procedure;
  struct vm_ext_object ext_object;
} vm_obj_value_t;

/* VM objects consist of a type specifier and its corresponding
   value representation. */
typedef struct vm_obj {
  vm_obj_value_t value;
  vm_obj_type_t type;
} vm_obj_t;

/* Part of the VM_TYPE_LIST representations, but this is defined after
   vm_obj_t because list items contain full VM objects. */
typedef struct vm_list_item {
  struct vm_obj obj;
  struct vm_list_item *next;
} vm_list_item_t;

/*
 * Definitions for external object types that can be defined by
 * VM libraries. These objects need special functions for deep-copying,
 * deallocation, and writing their textual representation.
 */
typedef struct vm_ext_type {
  void (*copy)(vm_obj_t *, vm_obj_t *);
  void (*deallocate)(vm_obj_t *);
  void (*write)(vm_port_t *port, vm_obj_t *);
  vm_ext_type_id_t type_id;
} vm_ext_type_t;

#endif /* !VM_OBJECTS_H */
