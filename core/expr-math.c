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

#include <math.h>

#include "vm-functions.h"

static vm_integer_t
i_abs(vm_integer_t a)
{
  return a >= 0 ? a : -a;
}

static vm_integer_t
i_gcd(vm_integer_t a, vm_integer_t b)
{
  vm_integer_t shift_result, old_a;
  uint8_t a_even, b_even;

  if(a == 0 || b == 0) {
    return a != b; /* GCD is zero if a equals b, otherwise it is 1. */
  }

  a = i_abs(a);
  b = i_abs(b);

  /* The Binary GCD algorithm. */

  shift_result = 0;
  while(a != b) {
    if(a == 1 || b == 1) {
      return 1 << shift_result;
    }

    a_even = ~a & 1;
    b_even = ~b & 1;

    if(a_even) {
      a /= 2;
      if(b_even) {
        b /= 2;
        shift_result++;
      }
    } else {
      if(b_even) {
        b /= 2;
      } else {
        if(a > b) {
          a = (a - b) / 2;
        } else {
          old_a = a;
          a = (b - a) / 2;
          b = old_a;
        }
      }
    }
  }

  return a << shift_result;
}

static vm_integer_t
i_lcm(vm_integer_t a, vm_integer_t b)
{
  vm_integer_t gcd;

  gcd = i_gcd(a, b);
  if(gcd == 0) {
    return 0;
  }

  /* Avoid overflow at the cost of an extra call to abs(). */
  return (i_abs(a) / gcd) * i_abs(b);
}

static vm_rational_t
r_add(vm_rational_t a, vm_rational_t b)
{
  vm_rational_t result;

  if(a.denominator == 0 || b.denominator == 0) {
    result.numerator = 0;
    result.denominator = 0;
    VM_DEBUG(VM_DEBUG_MEDIUM,
             "Attempt to add rational with a denominator of 0");
  } else if(a.denominator == b.denominator) {
    /* Avoid costly operations for integer arithmetic. */
    result.denominator = a.denominator;
    result.numerator = a.numerator + b.numerator;
  } else {
    result.denominator = i_lcm(a.denominator, b.denominator);
    result.numerator = a.numerator * (result.denominator / a.denominator) +
                     b.numerator * (result.denominator / b.denominator);
  }

  return result;
}

static vm_rational_t
r_subtract(vm_rational_t a, vm_rational_t b)
{
  b.numerator = -b.numerator;

  return r_add(a, b);
}

static vm_rational_t
r_multiply(vm_rational_t a, vm_rational_t b)
{
  vm_rational_t result;

  result.numerator = a.numerator * b.numerator;
  result.denominator = a.denominator * b.denominator;

  return result;
}

static vm_rational_t
r_div(vm_rational_t a, vm_rational_t b)
{
  vm_rational_t result;

  result.numerator = a.numerator * b.denominator;
  result.denominator = a.denominator * b.numerator;

  return result;
}

static void
r_normalize(vm_rational_t *rp)
{
  vm_integer_t divisor;

  if(rp->denominator < 0) {
    rp->denominator = -rp->denominator;
    rp->numerator = -rp->numerator;
  }

  if(rp->denominator != 1) {
    divisor = i_gcd(rp->numerator, rp->denominator);
    if(divisor != 0) {
      rp->numerator /= divisor;
      rp->denominator /= divisor;
    }
  }
}

static void
i_to_r(vm_integer_t i, vm_rational_t *r)
{
  r->numerator = i;
  r->denominator = 1;
}

static void
generate_result(vm_thread_t *thread, vm_rational_t r)
{
  vm_rational_t *rp;

  if(r.numerator == 0 || r.denominator == 1) {
    VM_PUSH_INTEGER(r.numerator);
  } else {
    rp = VM_MALLOC(sizeof(vm_rational_t));
    if(rp == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
    } else {
      *rp = r;
      r_normalize(rp);
      VM_PUSH_RATIONAL(rp);
    }
  }
}

VM_FUNCTION(add)
{
  vm_rational_t sum, r;

  sum.numerator = 0;
  sum.denominator = 1;

  while(argc-- > 0) {
    if(argv[argc].type == VM_TYPE_INTEGER) {
      i_to_r(argv[argc].value.integer, &r);
    } else {
      r = *argv[argc].value.rational;
    }
    sum = r_add(sum, r);
  }

  generate_result(thread, sum);
}

VM_FUNCTION(subtract)
{
  vm_rational_t diff, r;

  if(argv[0].type == VM_TYPE_RATIONAL) {
    diff = *argv[0].value.rational;
  } else {
    i_to_r(argv[0].value.integer, &diff);
  }

  if(argc == 1) {
    diff.numerator = -diff.numerator;
  } else {
    while(argc-- > 1) {
      if(argv[argc].type == VM_TYPE_INTEGER) {
	i_to_r(argv[argc].value.integer, &r);
      } else {
	r = *argv[argc].value.rational;
      }
      diff = r_subtract(diff, r);
    }
  }

  generate_result(thread, diff);
}

VM_FUNCTION(multiply)
{
  vm_rational_t product, r;

  product.numerator = product.denominator = 1;

  while(argc-- > 0) {
    switch(argv[argc].type) {
    case VM_TYPE_INTEGER:
      i_to_r(argv[argc].value.integer, &r);
      break;
    case VM_TYPE_RATIONAL:
      r = *argv[argc].value.rational;
      break;
    default:
      vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
      return;
    }

    if(r.numerator == 0) {
      product.numerator = 0;
      break;
    }

    product = r_multiply(product, r);
    r_normalize(&product);
  }

  generate_result(thread, product);
}

VM_FUNCTION(divide)
{
  vm_rational_t quotient, r;

  if(argv[0].type == VM_TYPE_INTEGER) {
    i_to_r(argv[0].value.integer, &quotient);
  } else if (argv[0].type == VM_TYPE_RATIONAL) {
    quotient = *argv[0].value.rational;
  } else {
    vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
    return;
  }

  while(argc-- > 1) {
    if(argv[argc].type == VM_TYPE_INTEGER) {
      i_to_r(argv[argc].value.integer, &r);
    } else {
      r = *argv[argc].value.rational;
    }

    if(r.numerator == 0) {
      vm_signal_error(thread, VM_ERROR_DIV0);
      return;
    }

    quotient = r_div(quotient, r);
    r_normalize(&quotient);
  }

  generate_result(thread, quotient);
}

VM_FUNCTION(gcd)
{
  vm_integer_t result;
  int i;

  result = argv[0].value.integer;
  for(i = 1; i < argc; i++) {
    result = i_gcd(result, argv[i].value.integer);
  }
  VM_PUSH_INTEGER(result);
}

VM_FUNCTION(lcm)
{
  vm_integer_t result;
  int i;

  result = argv[0].value.integer;
  for(i = 1; i < argc && result != 0; i++) {
    result = i_lcm(result, argv[i].value.integer);
  }
  VM_PUSH_INTEGER(result);
}

VM_FUNCTION(numerator)
{
  vm_rational_t r;

  if(argv[0].type == VM_TYPE_INTEGER) {
    i_to_r(argv[0].value.integer, &r);
  } else {
    r = *argv[0].value.rational;
  }
  VM_PUSH_INTEGER(r.numerator);
}

VM_FUNCTION(denominator)
{
  vm_rational_t r;

  if(argv[0].type == VM_TYPE_INTEGER) {
    i_to_r(argv[0].value.integer, &r);
  } else {
    r = *argv[0].value.rational;
  }
  VM_PUSH_INTEGER(r.denominator);
}

VM_FUNCTION(quotient)
{
  if(argv[1].value.integer == 0) {
    vm_signal_error(thread, VM_ERROR_DIV0);
  } else {
    VM_PUSH_INTEGER(argv[0].value.integer / argv[1].value.integer);
  }
}

VM_FUNCTION(remainder)
{
  vm_integer_t quotient, remainder;

  if(argv[1].value.integer == 0) {
    vm_signal_error(thread, VM_ERROR_DIV0);
  } else {
    quotient = argv[0].value.integer / argv[1].value.integer;
    remainder = argv[0].value.integer - quotient * argv[1].value.integer;
    VM_PUSH_INTEGER(remainder);
  }
}

VM_FUNCTION(modulo)
{
  if(argv[1].value.integer == 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
  } else {
    VM_PUSH_INTEGER(argv[0].value.integer % argv[1].value.integer);
  }
}

VM_FUNCTION(floor)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(floor(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(ceiling)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(ceil(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(round)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(round(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(truncate)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(trunc(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(exp)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(exp(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(log)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(log(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(sin)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(sin(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(cos)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(cos(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(tan)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(tan(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(asin)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(asin(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(acos)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(acos(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(atan)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(atan(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(sqrt)
{
#if VM_ENABLE_REALS
  vm_real_t x;

  x = argv[0].value.integer;
  VM_PUSH_REAL(sqrt(x));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(expt)
{
#if VM_ENABLE_REALS
  vm_real_t x, y;

  x = argv[0].value.integer;
  y = argv[1].value.integer;
  VM_PUSH_REAL(pow(x, y));
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(exact_to_inexact)
{
#if VM_ENABLE_REALS
  switch(argv[0].type) {
  case VM_TYPE_REAL:
    VM_PUSH(&argv[0]);
    break;
  case VM_TYPE_INTEGER:
    VM_PUSH_REAL(argv[0].value.integer);
    break;
  case VM_TYPE_RATIONAL:
    VM_PUSH_REAL((float)argv[0].value.rational->numerator /
                         argv[0].value.rational->denominator);
    break;
  default:
    break;
  }
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}

VM_FUNCTION(inexact_to_exact)
{
#if VM_ENABLE_REALS
  switch(argv[0].type) {
  case VM_TYPE_REAL:
    /* TODO: Implement a suitable algorithm. */
    vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
    break;
  case VM_TYPE_INTEGER:
  case VM_TYPE_RATIONAL:
    VM_PUSH(&argv[0]);
  default:
    break;
  }
#else
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
#endif /* VM_ENABLE_REALS */
}
