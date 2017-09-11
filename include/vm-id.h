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
 * 3. Neither the name of the author nor the names of the contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#ifndef VM_ID_H
#define VM_ID_H

#include <limits.h>
#include <stdint.h>

/* The invalid id consists of all bits set to 0 because a valid ID must
   have a nonce > 1. */
#define VM_ID_INVALID 0

/*
 * The invalid index consists of all bits set to 1 because a
 * valid index must below the highest possible index value, which
 * is currently UINT16T_MAX.
 */
#define VM_ID_INDEX_INVALID UINT16_MAX

typedef struct vm_id_gen {
  uint16_t random_offset;
  uint16_t nonce;
  uint16_t max_index;
  uint8_t index_bits;
} vm_id_gen_t;

typedef uint16_t vm_id_index_t;
typedef int32_t vm_id_t;

int vm_id_create_generator(vm_id_gen_t *, vm_id_t);
vm_id_t vm_id_new(vm_id_gen_t *, vm_id_index_t);
vm_id_index_t vm_id_index(vm_id_gen_t *, vm_id_t);

#endif /* !VM_ID_H */
