/*
 * Copyright (c) 2012-2017, RISE SICS AB.
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

#include "sys/energest.h"

#include "vm.h"
#include "vm-native.h"

PROCESS(vm_perfmon_process, VM_NAME" Monitor");

/* Print out performance data for each program at the following interval,
   which is expressed in seconds. */
#ifndef VM_PERF_MONITOR_INTERVAL
#define VM_PERF_MONITOR_INTERVAL 5
#endif

static void
print_perf_cpu(const vm_program_t *program)
{
  const vm_policy_rule_t *rule;

  printf("VPM %s CPU %u%% POLICY ", program->name,
         program->perf_attr.cpu_usage);
  if(program->policy != NULL &&
     (rule = program->policy->rules[VM_POLICY_TYPE_CPU]) != NULL) {
    printf("%u%%\n", rule->cpu.usage_percentage);
  } else {
    printf("<default>\n");
  }
}

unsigned
calculate_system_power(void)
{
  unsigned long e_cpu, e_lpm, e_transmit, e_listen;
  uint64_t power;

  /* TODO: Include the new Contiki-NG type ENERGEST_TYPE_DEEP_LPM. */

  e_cpu      = energest_type_time(ENERGEST_TYPE_CPU);
  e_lpm      = energest_type_time(ENERGEST_TYPE_LPM);
  e_transmit = energest_type_time(ENERGEST_TYPE_TRANSMIT);
  e_listen   = energest_type_time(ENERGEST_TYPE_LISTEN);

#if CONTIKI_TARGET_ZOUL
  power = (uint64_t)e_cpu * 13000;
  power += (uint64_t)e_lpm * 600;
  power += (uint64_t)e_transmit * 21000;
  power += (uint64_t)e_listen * 11000;
  power = 3 * (power / RTIMER_SECOND);
#else
  power = (uint64_t)e_cpu * 1800;
  power += (uint64_t)e_lpm * 55;
  power += (uint64_t)e_transmit * 17700;
  power += (uint64_t)e_listen * 20000;
  power = 3 * (power / RTIMER_SECOND);
#endif /* CONTIKI_TARGET_ZOUL */

  power /= clock_seconds();

  return power;
}

static void
print_perf_bandwidth(const vm_program_t *program)
{
  const vm_policy_rule_t *rule;
  unsigned long bandwidth;

  printf("VPM %s BANDWIDTH %lu POLICY ", program->name,
         (unsigned long)vm_filter_get(&program->perf_attr.bandwidth));
  if(program->policy != NULL &&
     (rule = program->policy->rules[VM_POLICY_TYPE_BANDWIDTH]) != NULL) {
    bandwidth = rule->bandwidth.throughput;
    if(rule->bandwidth.unit == VM_POLICY_UNIT_KBPS) {
      bandwidth *= 1000;
    }
    printf("%lu bps\n", bandwidth);
  } else {
    printf("<default>\n");
  }

}

static void
print_perf_power(const vm_program_t *program)
{
  const vm_policy_rule_t *rule;
  unsigned long power;

  printf("SPM POWER %u\n", calculate_system_power());
  printf("VPM %s POWER %u POLICY ", program->name,
         vm_native_calculate_power(program));
  if(program->policy != NULL &&
     (rule = program->policy->rules[VM_POLICY_TYPE_POWER]) != NULL) {
    power = rule->power.allocated_power;
    if(rule->power.unit == VM_POLICY_UNIT_MW) {
      power *= 1000;
    }
    printf("%lu uW\n", power);
  } else {
    printf("<default>\n");
  }
}

static void
print_perf_data(void)
{
  const vm_program_t *program;

  for(program = vm_get_programs(); program != NULL; program = program->next) {
    print_perf_bandwidth(program);
    print_perf_cpu(program);
    print_perf_power(program);
  }
}

static void
print_mem_stats(void)
{
  vm_memory_stats_t stats;

  /* Do not print statistics if no programs are running. */
  if(vm_get_programs() == NULL) {
    return;
  }

  vm_memory_get_stats(&stats);
  printf("MEM allocs %lu mempool_fwd %lu alloc_bytes %lu manual_deallocs %lu gc_deallocs %lu gc_invoc %lu\n",
         (unsigned long)stats.allocations,
         (unsigned long)stats.mempool_forwards,
         (unsigned long)stats.allocated_bytes,
         (unsigned long)stats.manual_deallocations,
         (unsigned long)stats.gc_deallocations,
         (unsigned long)stats.gc_invocations);
}

PROCESS_THREAD(vm_perfmon_process, ev, data)
{
  static struct etimer et;

  PROCESS_BEGIN();

  etimer_set(&et, CLOCK_SECOND * VM_PERF_MONITOR_INTERVAL);
  for(;;) {
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));
    print_perf_data();
    print_mem_stats();
    etimer_reset(&et);
  }

  PROCESS_END();
}
