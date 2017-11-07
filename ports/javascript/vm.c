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

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <emscripten.h>

#include "vm.h"
#include "vm-log.h"
#include "vm-macros.h"
#include "vm-native.h"

int
main(int argc, char *argv[])
{
#ifndef VM_BUNDLE
  int i;

  if(argc < 2) {
    fprintf(stderr, "Usage: %s <program> [<program> ...]\n", argv[0]);
    return EXIT_FAILURE;
  }
#endif

  EM_ASM(
    FS.mkdir('/apps');
    FS.mount(NODEFS, { root: '../../apps' }, '/apps');
  );

  if(vm_init() == 0) {
    fprintf(stderr, "Unable to initialize the VM\n");
    return EXIT_FAILURE;
  }

#ifdef VM_BUNDLE
  /* Load a single program from a designated area in memory. */
  vm_load_program(VM_MAKE_STRING(VM_BUNDLE));
#else
  /* Load the programs from the file system. */
  for(i = 1; i < argc; i++) {
    if(vm_load_program(argv[i]) == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Unable to load program %s", argv[i]);
    }
  }
#endif

  for(;;) {
    /* Main VM processing loop. */
    while(vm_run() != VM_RESULT_FINISHED);
    if(VM_ALWAYS_ON) {
      vm_native_poll();
    } else {
      break;
    }
  }

  vm_exit();

  return EXIT_SUCCESS;
}
