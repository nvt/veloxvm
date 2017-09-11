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

#ifndef VM_PERF_ATTR_H
#define VM_PERF_ATTR_H

#include "vm-filter.h"
#include "vm-types.h"

/*
 * Program-specific performance attributes measured during execution. These
 * attributes will be used by the policy enforcer to ensure that programs
 * do not use more resources than what is specified by their security policy.
 */

typedef struct vm_perf_attr {
  /* Start time of the program. */
  vm_time_t start_time;

  /* The time when the program last communicated through a port. */
  vm_time_t last_comm;

  /* Moving average of the communication bandwidth used by the program. */
  vm_filter_t bandwidth;

  /* Moving average of the power consumption, expressed in milliwatts. */
  vm_filter_t power;

  /* Power overuse, expressed in microwatts. */
  uint32_t power_overuse;

  /*
   * Time spent in different radio states because of the program's use
   * of the radio. This time is used to compute the power consumption of
   * the program.
   */
  uint32_t radio_rx_time;
  uint32_t radio_tx_time;

  /* Total number of system time ticks spent executing the program. */
  uint32_t cpu_time;

  /*
   * The number of instructions to execute in each invocation. The default
   * value is VM_EXEC_INSTR_PER_INVOCATION, which is used whenever the CPU
   * usage policy is not violated.
   */
  uint16_t exec_instr_per_invocation;

  /* Maximum CPU usage percentage observed for this program. */
  uint8_t max_cpu_usage;

  /* Latest CPU usage percentage observed for this program. */
  uint8_t cpu_usage;
} vm_perf_attr_t;

#endif /* !VM_PERF_ATTR_H */
