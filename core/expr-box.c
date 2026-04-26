/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB.
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
 * Author: Nicolas Tsiftes <nicolas.tsiftes@ri.se>
 */

#include <string.h>

#include "vm.h"
#include "vm-functions.h"

VM_FUNCTION(box)
{
  if(vm_box_create(&thread->result, &argv[0]) == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  }
}

VM_FUNCTION(box_ref)
{
  if(argv[0].type != VM_TYPE_BOX) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  if(argv[0].value.box == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  memcpy(&thread->result, &argv[0].value.box->value, sizeof(vm_obj_t));
}

VM_FUNCTION(box_set)
{
  if(argv[0].type != VM_TYPE_BOX) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  if(argv[0].value.box == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  memcpy(&argv[0].value.box->value, &argv[1], sizeof(vm_obj_t));
}
