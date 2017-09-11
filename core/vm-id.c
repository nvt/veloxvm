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

#include <stdlib.h>

#include "vm.h"
#include "vm-id.h"
#include "vm-log.h"

int
vm_id_create_generator(vm_id_gen_t *idg, vm_id_t max_index)
{
  if(max_index == VM_ID_INDEX_INVALID) {
    return 0;
  }

#if 0
  idg->index_bits = vm_highest_bit(max_index) + 1;
#else
  idg->index_bits = 16;
#endif
  idg->random_offset = VM_RANDOM_FUNCTION();
  idg->max_index = max_index;
  idg->nonce = VM_RANDOM_FUNCTION();

  VM_DEBUG(VM_DEBUG_MEDIUM, "New ID generator (%u, %u)",
           (unsigned)idg->random_offset, (unsigned)idg->max_index);

  return 1;
}

vm_id_t
vm_id_new(vm_id_gen_t *idg, vm_id_index_t index)
{
  vm_id_t id;

  if(index > idg->max_index) {
    return VM_ID_INVALID;
  }

  id = ((vm_id_t)index + idg->random_offset) << 15;

  ++idg->nonce;

  /* Ensure that the nonce is within at most 15 bits and above zero. */
  if(idg->nonce == 0 || idg->nonce >= 0x7fffU) {
    idg->nonce = 1;
  }

  id |= idg->nonce;

  return id;
}

vm_id_index_t
vm_id_index(vm_id_gen_t *idg, vm_id_t id)
{
  vm_id_index_t index;

  index = (id >> 15) - idg->random_offset;
  if(id == VM_ID_INVALID || index > idg->max_index) {
    return VM_ID_INDEX_INVALID;
  }

  VM_DEBUG(VM_DEBUG_HIGH, "ID gen %p: ID %lu => INDEX %lu",
     idg, (unsigned long)id, (unsigned long)index);

  return index;
}
