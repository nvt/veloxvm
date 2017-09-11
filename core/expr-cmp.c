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

  for(i = 1; i < argc; i++) {
    if(argv[i - 1].value.integer != argv[i].value.integer) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(different)
{
  vm_integer_t i;

  for(i = 1; i < argc; i++) {
    if(argv[i - 1].value.integer == argv[i].value.integer) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(less_than)
{
  vm_integer_t i;

  for(i = 1; i < argc; i++) {
    if(argv[i - 1].value.integer >= argv[i].value.integer) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(less_than_equal)
{
  vm_integer_t i;

  for(i = 1; i < argc; i++) {
    if(argv[i - 1].value.integer > argv[i].value.integer) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(greater_than)
{
  vm_integer_t i;

  for(i = 1; i < argc; i++) {
    if(argv[i - 1].value.integer <= argv[i].value.integer) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(greater_than_equal)
{
  vm_integer_t i;

  for(i = 1; i < argc; i++) {
    if(argv[i - 1].value.integer < argv[i].value.integer) {
      VM_PUSH_BOOLEAN(0);
      return;
    }
  }

  VM_PUSH_BOOLEAN(1);
}

VM_FUNCTION(zerop)
{
  vm_integer_t value;

  if(argv[0].type == VM_TYPE_RATIONAL) {
    value = argv[0].value.rational->numerator;
  } else {
    value = argv[0].value.integer;
  }

  VM_PUSH_BOOLEAN(value == 0);
}
