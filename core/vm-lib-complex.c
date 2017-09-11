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

/* Complex number library conforming to Scheme R5RS. */

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"

#include <math.h>
#include <string.h>

VM_DECLARE_FUNCTION(make_rectangular);
VM_DECLARE_FUNCTION(make_polar);
VM_DECLARE_FUNCTION(real_part);
VM_DECLARE_FUNCTION(imag_part);
VM_DECLARE_FUNCTION(magnitude);
VM_DECLARE_FUNCTION(angle);

static int load(vm_program_t *);
static int unload(vm_program_t *);

static const vm_procedure_t complex_operators[] = {
  VM_OPERATOR(make_rectangular, VM_TYPE_FLAG(VM_TYPE_INTEGER),
              VM_PROCEDURE_EVAL_ARGS, 2, 2),
  VM_OPERATOR(make_polar, VM_TYPE_FLAG(VM_TYPE_INTEGER),
              VM_PROCEDURE_EVAL_ARGS, 2, 2),
  VM_OPERATOR(real_part, VM_TYPE_FLAG(VM_TYPE_EXTERNAL) | VM_TYPE_FLAG_NUMBER,
              VM_PROCEDURE_EVAL_ARGS, 1, 1),
  VM_OPERATOR(imag_part, VM_TYPE_FLAG(VM_TYPE_EXTERNAL) | VM_TYPE_FLAG_NUMBER,
              VM_PROCEDURE_EVAL_ARGS, 1, 1),
  VM_OPERATOR(magnitude, VM_TYPE_FLAG(VM_TYPE_EXTERNAL) | VM_TYPE_FLAG_NUMBER,
              VM_PROCEDURE_EVAL_ARGS, 1, 1),
  VM_OPERATOR(angle, VM_TYPE_FLAG(VM_TYPE_EXTERNAL) | VM_TYPE_FLAG_NUMBER,
              VM_PROCEDURE_EVAL_ARGS, 1, 1)
};

vm_lib_t vm_lib_complex = {
  .name = "complex",
  .load = load,
  .unload = unload,
  .operators = complex_operators,
  .operator_count = ARRAY_SIZE(complex_operators),
  .symbols = (const char *[]){"make-rectangular", "make-polar", "real-part",
              "imag-part", "magnitude", "angle"},
  .symbol_count = 6
};

typedef struct vm_complex {
  vm_real_t real;
  vm_real_t imag;
} vm_complex_t;

static void complex_create(vm_obj_t *, vm_real_t, vm_real_t);
static void complex_copy(vm_obj_t *, vm_obj_t *);
static void complex_deallocate(vm_obj_t *);
static void complex_write(vm_port_t *, vm_obj_t *);

static vm_ext_type_t ext_type_complex = {
  .copy = complex_copy,
  .deallocate = complex_deallocate,
  .write = complex_write
};

#define VM_PUSH_COMPLEX(real, imag) \
          complex_create(&thread->result, (real), (imag))

static void
complex_create(vm_obj_t *dst, vm_real_t real, vm_real_t imag)
{
  vm_complex_t *complex;

  complex = VM_MALLOC(sizeof(vm_complex_t));
  if(complex != NULL) {
    dst->type = VM_TYPE_EXTERNAL;
    dst->value.ext_object.type = &ext_type_complex;
    dst->value.ext_object.opaque_data = complex;
    complex->real = real;
    complex->imag = imag;
  } else {
    memset(dst, 0, sizeof(vm_obj_t));
    dst->type = VM_TYPE_NONE;
  }
}

static void
complex_copy(vm_obj_t *dst, vm_obj_t *src)
{
  memcpy(dst, src, sizeof(vm_obj_t));
}

static void
complex_deallocate(vm_obj_t *obj)
{
  VM_FREE(obj->value.ext_object.opaque_data);
}

static void
complex_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_complex_t *complex;

  complex = obj->value.ext_object.opaque_data;

  vm_write(port, "%ld+%ldi", (long)complex->real, (long)complex->imag);
}

static int
load(vm_program_t *program)
{
#if VM_ENABLE_REALS
  if(ext_type_complex.type_id == 0) {
    vm_ext_type_register(&ext_type_complex);
    VM_DEBUG(VM_DEBUG_LOW, "Registering complex object type with ID %u",
             (unsigned)ext_type_complex.type_id);
  }

  return 1;
#else
  vm_signal_error(vm_current_thread(), VM_ERROR_UNIMPLEMENTED);
  return 0;
#endif
}

static int
unload(vm_program_t *program)
{
  return 1;
}

#if VM_ENABLE_REALS
static vm_real_t
get_real(vm_obj_t *obj)
{
  switch(obj->type) {
  case VM_TYPE_INTEGER:
    return obj->value.integer;
  case VM_TYPE_RATIONAL:
    return obj->value.rational->numerator /
           (float)obj->value.rational->denominator;
  case VM_TYPE_REAL:
    return obj->value.real;
  case VM_TYPE_EXTERNAL:
    if(obj->value.ext_object.type == &ext_type_complex) {
      vm_complex_t *complex = obj->value.ext_object.opaque_data;
      return complex->real;
    }
  default:
    VM_DEBUG(VM_DEBUG_MEDIUM, "Erroneous type to convert to a real: %d",
             (int)obj->type);
    return 0;
  }
}
#endif /* VM_ENABLE_REALS */

VM_FUNCTION(make_rectangular)
{
#if VM_ENABLE_REALS
  VM_PUSH_COMPLEX(get_real(&argv[0]), get_real(&argv[1]));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif
}

VM_FUNCTION(make_polar)
{
#if VM_ENABLE_REALS
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif
}

VM_FUNCTION(real_part)
{
#if VM_ENABLE_REALS
  if(argv[0].type == VM_TYPE_COMPLEX) {
/*    VM_PUSH_REAL(argv[0].value.complex.real);*/
    VM_PUSH_REAL(0);
  } else {
    VM_PUSH(&argv[0]);
  }
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif
}

VM_FUNCTION(imag_part)
{
#if VM_ENABLE_REALS
  if(argv[0].type == VM_TYPE_COMPLEX) {
    VM_PUSH_REAL(0);
  } else {
    VM_PUSH_INTEGER(0);
  }
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif
}

VM_FUNCTION(magnitude)
{
#if VM_ENABLE_REALS
  vm_complex_t *complex;
  vm_real_t real;
  vm_real_t imag;

  if(argv[0].type == VM_TYPE_EXTERNAL &&
     argv[0].value.ext_object.type == &ext_type_complex) {
    complex = argv[0].value.ext_object.opaque_data;
    real = complex->real;
    imag = complex->imag;
  } else {
    real = get_real(&argv[0]);
    imag = 0;
  }

  VM_PUSH_REAL(sqrt(pow(real, 2) + pow(imag, 2)));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif
}

VM_FUNCTION(angle)
{
#if VM_ENABLE_REALS
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif
}
