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

#include <ctype.h>

#include "vm-functions.h"

VM_FUNCTION(charp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_CHARACTER);
}

VM_FUNCTION(char_compare)
{
  VM_PUSH_INTEGER((unsigned)argv[0].value.character -
		  (unsigned)argv[1].value.character);
}

VM_FUNCTION(char_class)
{
  int ch;

  ch = argv[0].value.character;

  if(isdigit(ch)) {
    VM_PUSH_INTEGER(1);
  } else if(isblank(ch)) {
    VM_PUSH_INTEGER(0);
  } else if(isalpha(ch)) {
    if(isupper(ch)) {
      VM_PUSH_INTEGER(-1);
    } else {
      /* Lower case. */
      VM_PUSH_INTEGER(-2);
    }
  }
}

VM_FUNCTION(char_to_integer)
{
  VM_PUSH_INTEGER(argv[0].value.character);
}

VM_FUNCTION(integer_to_char)
{
  VM_PUSH_CHARACTER(argv[0].value.integer);
}

VM_FUNCTION(char_upcase)
{
  VM_PUSH_CHARACTER(toupper(argv[0].value.character));
}

VM_FUNCTION(char_downcase)
{
  VM_PUSH_CHARACTER(tolower(argv[0].value.character));
}
