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
#include "dev/watchdog.h"
#if WITH_POWERTRACE
#include "apps/powertrace/powertrace.h"
#endif

#include "vm.h"
#include "vm-file.h"
#include "vm-lib.h"
#include "vm-log.h"
#include "vm-native.h"

extern vm_lib_t vm_lib_leds;
extern vm_lib_t vm_lib_radio;
extern vm_lib_t vm_lib_rpl;

PROCESS(vm_process, VM_NAME);
AUTOSTART_PROCESSES(&vm_process);

extern const char vm_program_name[];

PROCESS_THREAD(vm_process, ev, data)
{
  static rtimer_clock_t start, end;
  static unsigned long start_sec, end_sec;
  static int iterations = 1;
  static unsigned int sched_counter;
  vm_result_t result;

  PROCESS_BEGIN();

#if WITH_POWERTRACE
  powertrace_start(CLOCK_SECOND * 10);
#endif

  if(!vm_init()) {
    VM_DEBUG(VM_DEBUG_LOW, "Unable to initialize the VM");
    PROCESS_EXIT();
  }

  vm_lib_register(&vm_lib_leds);
  vm_lib_register(&vm_lib_radio);
  vm_lib_register(&vm_lib_rpl);

  process_start(&vm_perfmon_process, NULL);

  while(iterations-- > 0) {
    sched_counter = 0;

    /* Load a program with privileged status (super user.) */
    if(vm_load_program(vm_program_name) == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Failed to load %s", vm_program_name);
      PROCESS_EXIT();
    }

    PROCESS_PAUSE();

    start_sec = clock_seconds();
    start = RTIMER_NOW();

    while(1) {
      result = vm_run();
      watchdog_periodic();
      sched_counter++;
      if(result == VM_RESULT_FINISHED) {
        if(VM_ALWAYS_ON) {
#if WITH_POWERTRACE
          powertrace_stop();
#endif
          PROCESS_YIELD();
        } else {
          break;
        }
      } else if(result == VM_RESULT_SLEEPING) {
        PROCESS_YIELD();
      } else {
        PROCESS_PAUSE();
      }
    }

    end = RTIMER_NOW();
    end_sec = clock_seconds();

    if(iterations == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Execution time: %ld ticks. The clock runs at %ld Hz.",
             (long)end - start, (long)RTIMER_SECOND);
      if(end_sec > start_sec + 1) {
        VM_DEBUG(VM_DEBUG_LOW, "Total execution time: %lu seconds.",
               end_sec - start_sec);
      }
      VM_DEBUG(VM_DEBUG_LOW, "The scheduler was invoked %u times.", sched_counter);
    }
  }

  vm_exit();

#if WITH_POWERTRACE
  powertrace_stop();
#endif

  PROCESS_END();
}
