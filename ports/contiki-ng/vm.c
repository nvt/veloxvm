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
 */

#include <stdio.h>

#include "contiki.h"
#include "serial-shell.h"

#include "vm.h"
#include "vm-file.h"
#include "vm-lib.h"
#include "vm-log.h"
#include "vm-native.h"

#ifdef VM_REPL_ENABLE
#include "vm-repl.h"
void vm_repl_coap_init(vm_program_t *program);

#if !CONTIKI_TARGET_NATIVE
/* On embedded targets the rpl-border-router/embedded module sets up
   the SLIP fallback interface so the host's tunslip6 can reach the
   device's IPv6 stack over /dev/ttyACM0 (or whichever UART/CDC the
   board exposes). The border-router process auto-requests an IPv6
   prefix from tunslip6 on startup. */
PROCESS_NAME(border_router_process);
#define VM_REPL_BR_AUTOSTART &border_router_process,
#else
#define VM_REPL_BR_AUTOSTART
#endif
#endif

extern vm_lib_t vm_lib_leds;
extern vm_lib_t vm_lib_radio;
extern vm_lib_t vm_lib_rpl;
extern vm_lib_t vm_lib_crypto;
extern vm_lib_t vm_lib_sensors;

void vm_shell_init(void);

PROCESS(vm_process, VM_NAME);
#ifdef VM_REPL_ENABLE
AUTOSTART_PROCESSES(VM_REPL_BR_AUTOSTART &vm_process);
#else
AUTOSTART_PROCESSES(&vm_process);
#endif

#ifndef VM_REPL_ENABLE
extern const char vm_program_name[];
#endif

PROCESS_THREAD(vm_process, ev, data)
{
  vm_result_t result;
#ifdef VM_REPL_ENABLE
  static vm_program_t *repl_program;
#endif

  PROCESS_BEGIN();

  if(!vm_init()) {
    VM_DEBUG(VM_DEBUG_LOW, "Unable to initialize the VM");
    PROCESS_EXIT();
  }

  vm_lib_register(&vm_lib_leds);
  vm_lib_register(&vm_lib_radio);
  vm_lib_register(&vm_lib_rpl);
  vm_lib_register(&vm_lib_crypto);
  vm_lib_register(&vm_lib_sensors);

#if VM_PERFMON_ENABLE
  process_start(&vm_perfmon_process, NULL);
#endif
  serial_shell_init();
  vm_shell_init();

#ifdef VM_REPL_ENABLE
  /* Create the REPL program once and hand it to the CoAP frontend.
     vm_repl_coap_init registers the resources and installs the
     console-writer hook so application output flows through IO_OUT
     notifications. */
  repl_program = vm_repl_program_create("repl", NULL);
  if(repl_program == NULL) {
    VM_DEBUG(VM_DEBUG_LOW, "Unable to create the REPL program");
    PROCESS_EXIT();
  }
  vm_repl_coap_init(repl_program);
#else
  if(vm_program_name[0] != '\0') {
    if(vm_load_program(vm_program_name) == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Failed to autoload %s", vm_program_name);
    }
  }
#endif

  while(1) {
    if(vm_get_programs() == NULL) {
      PROCESS_YIELD();
      continue;
    }

    result = vm_run();
    if(result == VM_RESULT_SLEEPING) {
      PROCESS_YIELD();
    } else {
      PROCESS_PAUSE();
    }
  }

  PROCESS_END();
}
