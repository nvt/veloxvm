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

VM_FUNCTION(equal)
{
  vm_integer_t i;
  vm_rational_t r1, r2;

  for(i = 1; i < argc; i++) {
    /* Convert both arguments to rationals for comparison */
    if(argv[i - 1].type == VM_TYPE_INTEGER) {
      r1.numerator = argv[i - 1].value.integer;
      r1.denominator = 1;
    } else if(argv[i - 1].type == VM_TYPE_RATIONAL) {
      r1 = *argv[i - 1].value.rational;
    } else {
      VM_PUSH_BOOLEAN(0);
      return;
    }

    if(argv[i].type == VM_TYPE_INTEGER) {
      r2.numerator = argv[i].value.integer;
      r2.denominator = 1;
    } else if(argv[i].type == VM_TYPE_RATIONAL) {
      r2 = *argv[i].value.rational;
    } else {
      VM_PUSH_BOOLEAN(0);
      return;
    }

    /* Compare using cross-multiplication to handle different denominators */
    if(r1.numerator * r2.denominator != r2.numerator * r1.denominator) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(different)
{
  vm_integer_t i;
  vm_rational_t r1, r2;

  for(i = 1; i < argc; i++) {
    /* Convert both arguments to rationals for comparison */
    if(argv[i - 1].type == VM_TYPE_INTEGER) {
      r1.numerator = argv[i - 1].value.integer;
      r1.denominator = 1;
    } else if(argv[i - 1].type == VM_TYPE_RATIONAL) {
      r1 = *argv[i - 1].value.rational;
    } else {
      VM_PUSH_BOOLEAN(0);
      return;
    }

    if(argv[i].type == VM_TYPE_INTEGER) {
      r2.numerator = argv[i].value.integer;
      r2.denominator = 1;
    } else if(argv[i].type == VM_TYPE_RATIONAL) {
      r2 = *argv[i].value.rational;
    } else {
      VM_PUSH_BOOLEAN(0);
      return;
    }

    /* Compare using cross-multiplication - return false if any pair is equal */
    if(r1.numerator * r2.denominator == r2.numerator * r1.denominator) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

/* Extract a numeric arg into a rational. Returns 0 on success, 1 if
   the arg type is not INTEGER or RATIONAL. The caller should have
   already filtered non-numeric types via valid_types at dispatch
   time, but argument counts include the operator slot's neighbours
   in some paths, so we re-check here. */
static int
extract_rational(vm_obj_t *obj, vm_rational_t *r)
{
  if(obj->type == VM_TYPE_INTEGER) {
    r->numerator = obj->value.integer;
    r->denominator = 1;
    return 0;
  } else if(obj->type == VM_TYPE_RATIONAL) {
    *r = *obj->value.rational;
    return 0;
  }
  return 1;
}

/* Cross-multiply two rationals to compare without dividing.
   r1 < r2 iff r1.num * r2.den < r2.num * r1.den when both
   denominators are positive (which they are, post-normalize).
   Returns -1, 0, or 1. */
static int
rational_compare(vm_rational_t *a, vm_rational_t *b)
{
  vm_integer_t lhs = a->numerator * b->denominator;
  vm_integer_t rhs = b->numerator * a->denominator;
  return (lhs < rhs) ? -1 : (lhs > rhs ? 1 : 0);
}

VM_FUNCTION(less_than)
{
  vm_integer_t i;
  vm_rational_t r1, r2;

  for(i = 1; i < argc; i++) {
    if(extract_rational(&argv[i - 1], &r1) ||
       extract_rational(&argv[i], &r2)) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    if(rational_compare(&r1, &r2) >= 0) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(less_than_equal)
{
  vm_integer_t i;
  vm_rational_t r1, r2;

  for(i = 1; i < argc; i++) {
    if(extract_rational(&argv[i - 1], &r1) ||
       extract_rational(&argv[i], &r2)) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    if(rational_compare(&r1, &r2) > 0) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(greater_than)
{
  vm_integer_t i;
  vm_rational_t r1, r2;

  for(i = 1; i < argc; i++) {
    if(extract_rational(&argv[i - 1], &r1) ||
       extract_rational(&argv[i], &r2)) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    if(rational_compare(&r1, &r2) <= 0) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(greater_than_equal)
{
  vm_integer_t i;
  vm_rational_t r1, r2;

  for(i = 1; i < argc; i++) {
    if(extract_rational(&argv[i - 1], &r1) ||
       extract_rational(&argv[i], &r2)) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    if(rational_compare(&r1, &r2) < 0) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(zerop)
{
  switch(argv[0].type) {
  case VM_TYPE_INTEGER:
    VM_PUSH_BOOLEAN(argv[0].value.integer == 0);
    return;
  case VM_TYPE_RATIONAL:
    VM_PUSH_BOOLEAN(argv[0].value.rational->numerator == 0);
    return;
#if VM_ENABLE_REALS
  case VM_TYPE_REAL:
    VM_PUSH_BOOLEAN(argv[0].value.real == 0.0);
    return;
#endif
  default:
    /* Dispatch already filters to NUMBER; anything else here means
       a numeric type the predicate does not implement (e.g. complex). */
    vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
    return;
  }
}
