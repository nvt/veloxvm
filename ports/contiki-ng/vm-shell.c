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
 *
 * Concurrency: all cmd_vm_* handlers execute between vm_run() invocations
 * because Contiki-NG schedules processes cooperatively. The VM scheduler
 * also clears its current_thread pointer before returning from vm_run()
 * (core/vm-sched.c) and re-walks loaded_programs from the head on each
 * call, so it is safe to mutate the program list (vm_load_program /
 * vm_unload_program) from a shell command without additional locking.
 * Anything ported to a preemptive RTOS would need to revisit this.
 */

#include "contiki.h"
#include "shell.h"
#include "shell-commands.h"
#include "sys/energest.h"

#include "vm.h"
#include "vm-filter.h"
#include "vm-log.h"
#include "vm-macros.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern struct process vm_process;

static const char *
thread_status_name(vm_thread_status_t s)
{
  switch(s) {
  case VM_THREAD_RUNNABLE: return "runnable";
  case VM_THREAD_WAITING:  return "waiting";
  case VM_THREAD_ERROR:    return "error";
  case VM_THREAD_EXITING:  return "exiting";
  case VM_THREAD_FINISHED: return "finished";
#ifdef VM_REPL_ENABLE
  case VM_THREAD_PARKED:   return "parked";
#endif
  }
  return "unknown";
}

/* Convert energest ticks (cpu_time, radio_*_time) to milliseconds.
   Cast through uint64_t because cpu_time is uint32_t and ENERGEST_SECOND
   on fast clocks (e.g. 32 kHz) makes the * 1000 step overflow 32 bits
   for runs of more than ~50 days. */
static unsigned long
ticks_to_ms(uint32_t ticks)
{
  return (unsigned long)((uint64_t)ticks * 1000U / ENERGEST_SECOND);
}

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
/* Parse a decimal thread ID from args. Returns 1 on success and writes the
   parsed value to *id, or 0 on failure (with an error printed). The full
   vm_id_t value is required; thread IDs include a nonce, so users copy them
   verbatim from vm-ps/vm-threads. */
static int
parse_thread_id(shell_output_func output, char *args, vm_id_t *id)
{
  char *end;
  long v;

  if(args == NULL || *args == '\0') {
    SHELL_OUTPUT(output, "Missing thread ID\n");
    return 0;
  }
  v = strtol(args, &end, 10);
  if(end == args || (*end != '\0' && *end != ' ')) {
    SHELL_OUTPUT(output, "Bad thread ID: %s\n", args);
    return 0;
  }
  *id = (vm_id_t)v;
  return 1;
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
  SHELL_OUTPUT(output, "-- cpu_time:        %lu ms\n",
               ticks_to_ms(p->perf_attr.cpu_time));
  SHELL_OUTPUT(output, "-- cpu_usage:       %u%% (max %u%%)\n",
               p->perf_attr.cpu_usage, p->perf_attr.max_cpu_usage);
  SHELL_OUTPUT(output, "-- exec_instr/inv:  %u\n",
               p->perf_attr.exec_instr_per_invocation);
  SHELL_OUTPUT(output, "-- radio rx/tx:     %lu / %lu ms\n",
               ticks_to_ms(p->perf_attr.radio_rx_time),
               ticks_to_ms(p->perf_attr.radio_tx_time));
  SHELL_OUTPUT(output, "-- bandwidth (avg): %lu B/s\n",
               (unsigned long)vm_filter_get(&p->perf_attr.bandwidth));
  SHELL_OUTPUT(output, "-- power (avg):     %lu mW\n",
               (unsigned long)vm_filter_get(&p->perf_attr.power));
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
static
PT_THREAD(cmd_vm_threads(struct pt *pt, shell_output_func output, char *args))
{
  vm_program_t *p;
  vm_thread_t *t;
  unsigned i;
  char *next_args;

  PT_BEGIN(pt);

  SHELL_ARGS_INIT(args, next_args);
  SHELL_ARGS_NEXT(args, next_args);
  p = find_program_arg(output, args);
  if(p == NULL) {
    PT_EXIT(pt);
  }

  SHELL_OUTPUT(output, "Threads for %s:\n", p->name);
  SHELL_OUTPUT(output, "ID         STATUS    SCHEDS    CALLS     EXPR\n");
  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    t = vm_thread_get_by_index(i);
    if(t == NULL || t->program != p) {
      continue;
    }
    SHELL_OUTPUT(output, "%-10ld %-9s %-9lu %-9lu %d\n",
                 (long)t->id,
                 thread_status_name(t->status),
                 (unsigned long)t->stats.schedulings,
                 (unsigned long)t->stats.function_calls,
                 t->expr ? (int)t->expr->expr_id : -1);
  }

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_kill(struct pt *pt, shell_output_func output, char *args))
{
  vm_id_t id;
  char *next_args;

  PT_BEGIN(pt);

  SHELL_ARGS_INIT(args, next_args);
  SHELL_ARGS_NEXT(args, next_args);
  if(!parse_thread_id(output, args, &id)) {
    PT_EXIT(pt);
  }

  if(vm_thread_kill(id) == 0) {
    SHELL_OUTPUT(output, "No such thread: %ld\n", (long)id);
  } else {
    SHELL_OUTPUT(output, "Killed thread %ld\n", (long)id);
  }

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_stack(struct pt *pt, shell_output_func output, char *args))
{
  vm_id_t id;
  vm_thread_t *t;
  char *next_args;

  PT_BEGIN(pt);

  SHELL_ARGS_INIT(args, next_args);
  SHELL_ARGS_NEXT(args, next_args);
  if(!parse_thread_id(output, args, &id)) {
    PT_EXIT(pt);
  }
  t = vm_thread_get(id);
  if(t == NULL) {
    SHELL_OUTPUT(output, "No such thread: %ld\n", (long)id);
    PT_EXIT(pt);
  }

  /* vm_print_stack_trace writes via VM_PRINTF (stdout). On Contiki-NG
     stdout is the same UART as the shell, so the trace is interleaved
     with shell output. A future port that decouples them will need a
     shell-output-aware variant. */
  vm_print_stack_trace(t);

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_policy(struct pt *pt, shell_output_func output, char *args))
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
  if(p->policy == NULL) {
    SHELL_OUTPUT(output, "%s has no policy attached\n", p->name);
    PT_EXIT(pt);
  }

  SHELL_OUTPUT(output, "Policy for %s:\n", p->name);
  /* Goes to VM_PRINTF -- see note in cmd_vm_stack. */
  vm_policy_print(p->policy);

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_gc(struct pt *pt, shell_output_func output, char *args))
{
  vm_memory_stats_t before;
  vm_memory_stats_t after;

  PT_BEGIN(pt);

  vm_memory_get_stats(&before);
  vm_gc_force();
  vm_memory_get_stats(&after);

  SHELL_OUTPUT(output, "GC forced:\n");
  SHELL_OUTPUT(output, "-- invocations:  %lu -> %lu\n",
               (unsigned long)before.gc_invocations,
               (unsigned long)after.gc_invocations);
  SHELL_OUTPUT(output, "-- deallocs:     %lu -> %lu (delta %lu)\n",
               (unsigned long)before.gc_deallocations,
               (unsigned long)after.gc_deallocations,
               (unsigned long)(after.gc_deallocations -
                               before.gc_deallocations));
  SHELL_OUTPUT(output, "-- peak heap:    %lu\n",
               (unsigned long)after.peak_heap_allocations);

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_devices(struct pt *pt, shell_output_func output, char *args))
{
  vm_device_t *d;
  char io[4];

  PT_BEGIN(pt);

  SHELL_OUTPUT(output, "FLG  IO   NAME\n");
  for(d = vm_device_get_all(); d != NULL; d = d->next) {
    int n = 0;
    if(d->flags & VM_PORT_FLAG_INPUT) {
      io[n++] = 'r';
    }
    if(d->flags & VM_PORT_FLAG_OUTPUT) {
      io[n++] = 'w';
    }
    if(n == 0) {
      io[n++] = '-';
    }
    io[n] = '\0';
    SHELL_OUTPUT(output, "0x%02x %-4s %s\n",
                 d->flags, io, d->name ? d->name : "(anon)");
  }

  PT_END(pt);
}
/*---------------------------------------------------------------------------*/
static
PT_THREAD(cmd_vm_version(struct pt *pt, shell_output_func output, char *args))
{
  PT_BEGIN(pt);

  SHELL_OUTPUT(output, "%s %d.%d (%s)\n",
               VM_NAME, VM_VERSION_MAJOR, VM_VERSION_MINOR,
               VM_MAKE_STRING(VM_PORT));
  SHELL_OUTPUT(output, "Build flags:\n");
  SHELL_OUTPUT(output, "-- VM_DEBUG_LEVEL:           %d\n", VM_DEBUG_LEVEL);
  SHELL_OUTPUT(output, "-- VM_SUPERUSER_MODE:        %d\n", VM_SUPERUSER_MODE);
  SHELL_OUTPUT(output, "-- VM_ALWAYS_ON:             %d\n", VM_ALWAYS_ON);
  SHELL_OUTPUT(output, "-- VM_SERVER:                %d\n", VM_SERVER);
  SHELL_OUTPUT(output, "-- VM_INSTRUCTION_PROFILING: %d\n",
               VM_INSTRUCTION_PROFILING);
  SHELL_OUTPUT(output, "-- VM_THREAD_AMOUNT:         %d\n", VM_THREAD_AMOUNT);

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
  { "vm-unload",  cmd_vm_unload,
    "'> vm-unload <name>': Unload a running program" },
  { "vm-threads", cmd_vm_threads,
    "'> vm-threads <name>': Show VM threads for a program" },
  { "vm-kill",    cmd_vm_kill,
    "'> vm-kill <id>': Kill a single VM thread by ID" },
  { "vm-stack",   cmd_vm_stack,
    "'> vm-stack <id>': Print stack trace for a VM thread" },
  { "vm-policy",  cmd_vm_policy,
    "'> vm-policy <name>': Show the security policy for a program" },
  { "vm-gc",      cmd_vm_gc,
    "'> vm-gc': Force a garbage collection cycle and report stats" },
  { "vm-devices", cmd_vm_devices,
    "'> vm-devices': List devices the VM has registered" },
  { "vm-version", cmd_vm_version,
    "'> vm-version': Show VM version, port, and build flags" },
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
