/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
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

/*
 * VeloxVM management shell commands for Contiki-NG.
 *
 * Wires a small subset of the VM control surface into the Contiki-NG
 * shell so an operator can list, inspect, load, and unload programs at
 * runtime without rebuilding the firmware.
 */

#include "contiki.h"
#include "shell.h"
#include "shell-commands.h"

#include "vm.h"
#include "vm-log.h"

#include <stdio.h>
#include <string.h>

extern struct process vm_process;

static vm_program_t *
find_program_arg(shell_output_func output, char *args)
{
  vm_program_t *p;

  if(args == NULL || *args == '\0') {
    SHELL_OUTPUT(output, "Missing program name\n");
    return NULL;
  }
  p = vm_find_program(args);
  if(p == NULL) {
    SHELL_OUTPUT(output, "No such program: %s\n", args);
  }
  return p;
}

/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_ps(struct pt *pt, shell_output_func output, char *args))
{
  vm_program_t *p;

  PT_BEGIN(pt);

  SHELL_OUTPUT(output, "ID  THR  FLG  CPU%%  NAME\n");
  for(p = vm_get_programs(); p != NULL; p = p->next) {
    SHELL_OUTPUT(output, "%-3ld %-4u 0x%02x %-5u %s\n",
                 (long)p->program_id,
                 p->nthreads,
                 p->flags,
                 p->perf_attr.cpu_usage,
                 p->name ? p->name : "(anon)");
  }

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_mem(struct pt *pt, shell_output_func output, char *args))
{
  vm_memory_stats_t s;

  PT_BEGIN(pt);

  vm_memory_get_stats(&s);
  SHELL_OUTPUT(output, "VM memory:\n");
  SHELL_OUTPUT(output, "-- allocations:        %lu\n",
               (unsigned long)s.allocations);
  SHELL_OUTPUT(output, "-- allocated bytes:    %lu\n",
               (unsigned long)s.allocated_bytes);
  SHELL_OUTPUT(output, "-- manual deallocs:    %lu\n",
               (unsigned long)s.manual_deallocations);
  SHELL_OUTPUT(output, "-- GC deallocs:        %lu\n",
               (unsigned long)s.gc_deallocations);
  SHELL_OUTPUT(output, "-- GC invocations:     %lu\n",
               (unsigned long)s.gc_invocations);
  SHELL_OUTPUT(output, "-- mempool forwards:   %lu\n",
               (unsigned long)s.mempool_forwards);
  SHELL_OUTPUT(output, "-- peak heap allocs:   %lu\n",
               (unsigned long)s.peak_heap_allocations);

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_perf(struct pt *pt, shell_output_func output, char *args))
{
  vm_program_t *p;
  char *next_args;

  PT_BEGIN(pt);

  SHELL_ARGS_INIT(args, next_args);
  SHELL_ARGS_NEXT(args, next_args);
  p = find_program_arg(output, args);
  if(p == NULL) {
    PT_EXIT(pt);
  }

  SHELL_OUTPUT(output, "Perf for %s:\n", p->name);
  SHELL_OUTPUT(output, "-- cpu_time:        %lu ticks\n",
               (unsigned long)p->perf_attr.cpu_time);
  SHELL_OUTPUT(output, "-- cpu_usage:       %u%% (max %u%%)\n",
               p->perf_attr.cpu_usage, p->perf_attr.max_cpu_usage);
  SHELL_OUTPUT(output, "-- exec_instr/inv:  %u\n",
               p->perf_attr.exec_instr_per_invocation);
  SHELL_OUTPUT(output, "-- radio rx/tx:     %lu / %lu ticks\n",
               (unsigned long)p->perf_attr.radio_rx_time,
               (unsigned long)p->perf_attr.radio_tx_time);
  SHELL_OUTPUT(output, "-- power overuse:   %lu uW\n",
               (unsigned long)p->perf_attr.power_overuse);

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_load(struct pt *pt, shell_output_func output, char *args))
{
  char *next_args;
  int rv;

  PT_BEGIN(pt);

  SHELL_ARGS_INIT(args, next_args);
  SHELL_ARGS_NEXT(args, next_args);
  if(args == NULL || *args == '\0') {
    SHELL_OUTPUT(output, "Usage: vm-load <file>\n");
    PT_EXIT(pt);
  }

  rv = vm_load_program(args);
  if(rv == 0) {
    SHELL_OUTPUT(output, "Failed to load %s\n", args);
  } else {
    SHELL_OUTPUT(output, "Loaded %s\n", args);
    /* Wake the VM process so it picks up the new program. */
    process_poll(&vm_process);
  }

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_unload(struct pt *pt, shell_output_func output, char *args))
{
  vm_program_t *p;
  char *next_args;

  PT_BEGIN(pt);

  SHELL_ARGS_INIT(args, next_args);
  SHELL_ARGS_NEXT(args, next_args);
  p = find_program_arg(output, args);
  if(p == NULL) {
    PT_EXIT(pt);
  }

  if(vm_unload_program(p) == 0) {
    SHELL_OUTPUT(output, "Unload failed\n");
  } else {
    SHELL_OUTPUT(output, "Unloaded %s\n", args);
  }

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static const struct shell_command_t vm_shell_commands[] = {
  { "vm-ps",     cmd_vm_ps,
    "'> vm-ps': List loaded VM programs" },
  { "vm-mem",    cmd_vm_mem,
    "'> vm-mem': Show VM memory and GC statistics" },
  { "vm-perf",   cmd_vm_perf,
    "'> vm-perf <name>': Show per-program performance attributes" },
  { "vm-load",   cmd_vm_load,
    "'> vm-load <file>': Load a bytecode image from CFS" },
  { "vm-unload", cmd_vm_unload,
    "'> vm-unload <name>': Unload a running program" },
  { NULL, NULL, NULL },
};

static struct shell_command_set_t vm_shell_command_set = {
  .next = NULL,
  .commands = vm_shell_commands,
};

void
vm_shell_init(void)
{
  shell_command_set_register(&vm_shell_command_set);
}
