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
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include <stdio.h>

#include "vm.h"
#include "vm-filter.h"
#include "vm-log.h"
#include "vm-native.h"

#if VM_SUPERUSER_MODE
#warning Security policy enforcement is disabled.
#else
static void
construct_exception_object(vm_obj_t *obj, vm_policy_type_t type,
                           const char *message)
{
  obj->type = VM_TYPE_INTEGER;
  obj->value.integer = 1000000 + type; /* Temporary hack. */
}

static vm_policy_reaction_t
get_policy_reaction(vm_policy_type_t type, vm_policy_rule_t *rule)
{
  return rule != NULL ? rule->reaction : vm_policy_default_reaction(type);
}

static void
reaction_exception(vm_thread_t *thread, vm_policy_type_t type,
		   vm_policy_rule_t *rule, const char *message)
{
  vm_obj_t exception_object;

  construct_exception_object(&exception_object, type, message);

  vm_raise_exception(thread, &exception_object);
}

static void
reaction_report(vm_thread_t *thread, vm_policy_type_t type,
                vm_policy_rule_t *rule)
{
}

static void
reaction_slowdown(vm_thread_t *thread, vm_policy_type_t type,
                  vm_policy_rule_t *rule)
{
  if(IS_CLEAR(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN);
    VM_DEBUG(VM_DEBUG_MEDIUM, "Forcing the app to slow down");
  }
}

static void
reaction_kill(vm_thread_t *thread, vm_policy_type_t type,
	      vm_policy_rule_t *rule, const char *message)
{
  vm_signal_error(thread, VM_ERROR_POLICY);
  vm_set_error_string(thread, message);
}

static void
policy_violation(vm_thread_t *thread, vm_policy_type_t type,
                 vm_policy_rule_t *rule, const char *message)
{
  vm_policy_reaction_t reaction;

  reaction = get_policy_reaction(type, rule);

  switch(reaction) {
  case VM_POLICY_REACTION_EXCEPTION:
    reaction_exception(thread, type, rule, message);
    break;
  case VM_POLICY_REACTION_REPORT:
    reaction_report(thread, type, rule);
    break;
  case VM_POLICY_REACTION_SLOWDOWN:
    reaction_slowdown(thread, type, rule);
    break;
  case VM_POLICY_REACTION_KILL:
    reaction_kill(thread, type, rule, message);
  default:
    VM_DEBUG(VM_DEBUG_LOW, "Illicit policy reaction: %d", (int)reaction);
    break;
  }
}
#endif /* VM_SUPERUSER_MODE */

vm_boolean_t
vm_policy_check_bandwidth(vm_thread_t *thread)
{
#if !VM_SUPERUSER_MODE
  vm_policy_rule_t *rule;
  uint32_t bps;
#endif

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
    VM_PRINTF("SYSTRACE %s %s\n", thread->program->name, "BANDWIDTH");
    return VM_TRUE;
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  rule = thread->program->policy->rules[VM_POLICY_TYPE_BANDWIDTH];
  if(rule != NULL) {
    bps = vm_filter_get(&thread->program->perf_attr.bandwidth);
    if(bps > rule->bandwidth.throughput) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "Bandwidth limit exceeded (%u > %u)",
               (unsigned)bps, (unsigned)rule->bandwidth.throughput);

      policy_violation(thread, VM_POLICY_TYPE_NET,
                       rule, "bandwidth limit exceeded");
      return VM_FALSE;
    }
  }

  CLEAR(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN);

  return VM_TRUE;
#endif /* VM_SUPERUSER_MODE */
}

vm_boolean_t
vm_policy_check_cpu(vm_program_t *program, unsigned cpu_usage)
{
#if VM_DEBUG
  static unsigned long x;
#endif
#if !VM_SUPERUSER_MODE
  vm_policy_rule_t *cpu_rule;
#endif

  program->perf_attr.cpu_usage = cpu_usage;

  if(cpu_usage > program->perf_attr.max_cpu_usage) {
    program->perf_attr.max_cpu_usage = cpu_usage;

    if(IS_SET(program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
      VM_PRINTF("SYSTRACE %s %s %u\n", program->name, "CPU",
		(unsigned)program->perf_attr.max_cpu_usage);
      return VM_TRUE;
    }
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  cpu_rule = program->policy->rules[VM_POLICY_TYPE_CPU];
  if(cpu_rule != NULL)  {
#if VM_DEBUG
    if((++x % 128) == 0)
       VM_PRINTF("x %lu, usage %u%%, limit %u%% instr %u\n",
		 x, cpu_usage, cpu_rule->cpu.usage_percentage,
		 program->perf_attr.exec_instr_per_invocation);
#endif
    if(cpu_usage > cpu_rule->cpu.usage_percentage) {
      /*
       * The CPU usage policy is violated. Decrease the number of
       * instructions that we execute each time we invoke a program
       * thread.
       *
       * The number may not go below 1, because the performance
       * attribute are only updated when the program is executed.
       */
      if(program->perf_attr.exec_instr_per_invocation > 10) {
	program->perf_attr.exec_instr_per_invocation =
	  (7 * program->perf_attr.exec_instr_per_invocation) / 10;
      } else {
	/* The overhead of having an IPI value below 10 is too high.
	   We let the current thread sleep instead to force its CPU
	   usage to go below the limit. */
	vm_native_sleep(vm_current_thread(),
			20 + 5 * (cpu_usage - cpu_rule->cpu.usage_percentage));
      }
      return VM_FALSE;
    } else if(cpu_usage + 2 < cpu_rule->cpu.usage_percentage) {
      /*
       * The CPU usage policy is not violated, and is within a 2
       * percentage point safety margin from the policy
       * limit. Increase the number described above to at most the
       * configured VM_EXEC_INSTR_PER_INVOCATION.
       */
      if(program->perf_attr.exec_instr_per_invocation <=
	 (9 * VM_EXEC_INSTR_PER_INVOCATION) / 10) {
	program->perf_attr.exec_instr_per_invocation =
	  (12 * program->perf_attr.exec_instr_per_invocation / 10);
      } else {
	program->perf_attr.exec_instr_per_invocation =
	  VM_EXEC_INSTR_PER_INVOCATION;
      }
    }
  } else {
    policy_violation(vm_current_thread(), VM_POLICY_TYPE_CPU, NULL, NULL);
  }

  return VM_TRUE;
#endif /* VM_SUPERUSER_MODE */
}

vm_boolean_t
vm_policy_check_file(vm_thread_t *thread, const char *path, unsigned mode)
{
#if !VM_SUPERUSER_MODE
  const vm_policy_t *policy;
  vm_policy_rule_t *rule;
#endif

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
    VM_PRINTF("SYSTRACE %s %s %s %c\n", thread->program->name, "FS",
	      path, mode == VM_PORT_FLAG_INPUT ? 'r' : 'w');
    return VM_TRUE;
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  policy = thread->program->policy;
  rule = NULL;
  if(policy != NULL) {
    for(rule = policy->rules[VM_POLICY_TYPE_FILE];
	rule != NULL;
	rule = rule->next) {
      if(strcmp(path, rule->file.path) == 0) {
	return VM_TRUE;
      }
    }
  }

  policy_violation(thread, VM_POLICY_TYPE_FILE, rule, path);

  return VM_FALSE;
#endif /* VM_SUPERUSER_MODE */
}

vm_boolean_t
vm_policy_check_net(vm_thread_t *thread,
		    const vm_vector_t *addr,
		    unsigned port)
{
#if !VM_SUPERUSER_MODE
  int i;
  const vm_policy_t *policy;
  vm_policy_rule_t *rule;
  uint8_t address[16];
#endif

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
    VM_PRINTF("SYSTRACE %s %s %u\n", thread->program->name, "NET", port);
    return VM_TRUE;
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  for(i = 0; i < 16; i += 2) {
    address[i] = (uint8_t)(addr->elements[i / 2].value.integer >> 8) & 0xff;
    address[i + 1] = (uint8_t)(addr->elements[i / 2].value.integer & 0xff);
  }

  policy = thread->program->policy;
  rule = NULL;
  if(policy != NULL) {
    for(rule = policy->rules[VM_POLICY_TYPE_NET];
	rule != NULL;
	rule = rule->next) {
      if(rule->net.port == port) {
	VM_DEBUG(VM_DEBUG_MEDIUM, "Found a rule with a matching port (%u)\n",
		 port);
	if(memcmp(address, rule->net.address, 16) == 0) {
	  VM_DEBUG(VM_DEBUG_MEDIUM, "IP addresses match in the rule\n");
	  return VM_TRUE;
	}
      }
    }
  }

  policy_violation(thread, VM_POLICY_TYPE_NET, rule, "connection denied");

  return VM_FALSE;
#endif /* VM_SUPERUSER_MODE */
}

vm_boolean_t
vm_policy_check_power(vm_thread_t *thread)
{
  uint32_t power;
#if !VM_SUPERUSER_MODE
  const vm_policy_t *policy;
  vm_policy_rule_t *rule;
  uint32_t allocated_power;
  uint32_t power_overuse;
  uint32_t power_limit;
  uint8_t slowdown_enabled;
#endif

  power = vm_native_calculate_power(thread->program);

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
    VM_PRINTF("SYSTRACE %s %s\n", thread->program->name, "POWER %u", power);
    return VM_TRUE;
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  policy = thread->program->policy;
  if(policy == NULL) {
    return VM_TRUE;
  }

  rule = policy->rules[VM_POLICY_TYPE_POWER];
  if(rule == NULL) {
    return VM_TRUE;
  }

  allocated_power = rule->power.allocated_power;
  if(rule->power.unit == VM_POLICY_UNIT_MW) {
    allocated_power *= 1000;
  }

  /* Add a 10% margin of error to the power enforcement. */
  if(allocated_power > 10) {
    allocated_power = (90 * allocated_power) / 100;
  }

  slowdown_enabled = IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN);
  power_overuse = thread->program->perf_attr.power_overuse;

  if(power_overuse >= allocated_power) {
    power_limit = 1;
  } else {
    power_limit = allocated_power - power_overuse;
  }

  if(power <= power_limit) {
    thread->program->perf_attr.power_overuse = 0;
    if(slowdown_enabled) {
      CLEAR(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN);
      VM_DEBUG(VM_DEBUG_MEDIUM,
               "No longer exceeding the power budget (%lu <= %lu)",
               (unsigned long)power, (unsigned long)power_limit);
    }
    return VM_TRUE;
  }

  if(!slowdown_enabled) {
    policy_violation(thread, VM_POLICY_TYPE_POWER,
                     rule, "power limit surpassed");
  }

  if(power_overuse) {
    thread->program->perf_attr.power_overuse = (95 * power_overuse) / 100;
    if(thread->program->perf_attr.power_overuse == power_overuse) {
      thread->program->perf_attr.power_overuse--;
    }
    VM_DEBUG(VM_DEBUG_HIGH, "Power overuse decreased to %lu",
             (unsigned long)thread->program->perf_attr.power_overuse);
  } else {
    thread->program->perf_attr.power_overuse = power - power_limit;
    VM_DEBUG(VM_DEBUG_HIGH, "power overuse set to %lu",
             (unsigned long)thread->program->perf_attr.power_overuse);
  }

  return VM_FALSE;
#endif /* VM_SUPERUSER_MODE */
}

vm_boolean_t
vm_policy_check_resources(vm_thread_t *thread, unsigned flags)
{
#if !VM_SUPERUSER_MODE
  const vm_policy_t *policy;
  vm_policy_rule_t *rule;
#endif

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
    VM_PRINTF("SYSTRACE %s %s ", thread->program->name, "RESOURCES");
    if(flags & VM_POLICY_RESOURCE_SUPERUSER) {
      VM_PRINTF("SUPERUSER\n");
    }
    if(flags & VM_POLICY_RESOURCE_CONSOLE) {
      VM_PRINTF("CONSOLE\n");
    }
    if(flags & VM_POLICY_RESOURCE_DNS) {
      VM_PRINTF("DNS\n");
    }
    if(flags & VM_POLICY_RESOURCE_STATS) {
      VM_PRINTF("STATS\n");
    }
    if(flags & VM_POLICY_RESOURCE_IPC) {
      VM_PRINTF("IPC\n");
    }
    return VM_TRUE;
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  policy = thread->program->policy;
  rule = NULL;
  if(policy != NULL) {
    rule = policy->rules[VM_POLICY_TYPE_RESOURCES];
    /* The superuser resource policy gives access to everything. */
    if(rule != NULL &&
       (rule->resources.resource_access & VM_POLICY_RESOURCE_SUPERUSER ||
	(rule->resources.resource_access & flags) == flags)) {
      return VM_TRUE;
    }
  }

  policy_violation(thread, VM_POLICY_TYPE_RESOURCES,
		   rule, "resource access denied");

  return VM_FALSE;
#endif /* VM_SUPERUSER_MODE */
}

vm_boolean_t
vm_policy_check_threads(vm_thread_t *thread)
{
#if !VM_SUPERUSER_MODE
  const vm_policy_t *policy;
  vm_policy_rule_t *rule;
#endif

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SYSTRACE)) {
    VM_PRINTF("SYSTRACE %s %s %u\n", thread->program->name, "THREADS",
	      (unsigned)thread->program->nthreads);
    return VM_TRUE;
  }

#if VM_SUPERUSER_MODE
  return VM_TRUE;
#else
  policy = thread->program->policy;
  rule = NULL;
  if(policy != NULL) {
    rule = policy->rules[VM_POLICY_TYPE_THREADS];
    if(rule != NULL && rule->threads.limit > thread->program->nthreads) {
      return VM_TRUE;
    }
  }

  policy_violation(thread, VM_POLICY_TYPE_THREADS,
		   rule, "thread limit surpassed");

  return VM_FALSE;
#endif /* VM_SUPERUSER_MODE */
}
