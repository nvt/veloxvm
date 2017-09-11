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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"
#include "vm-log.h"

struct vm_policy_reaction_rule {
  vm_policy_type_t type;
  vm_policy_reaction_t reaction;
};

struct vm_policy_reaction_rule default_reaction_rules[] = {
  {VM_POLICY_TYPE_BANDWIDTH, VM_POLICY_REACTION_SLOWDOWN},
  {VM_POLICY_TYPE_CPU, VM_POLICY_REACTION_SLOWDOWN},
  {VM_POLICY_TYPE_FILE, VM_POLICY_REACTION_EXCEPTION},
  {VM_POLICY_TYPE_MEMORY, VM_POLICY_REACTION_EXCEPTION},
  {VM_POLICY_TYPE_NET, VM_POLICY_REACTION_EXCEPTION},
  {VM_POLICY_TYPE_POWER, VM_POLICY_REACTION_SLOWDOWN},
  {VM_POLICY_TYPE_RESOURCES, VM_POLICY_REACTION_EXCEPTION},
  {VM_POLICY_TYPE_THREADS, VM_POLICY_REACTION_EXCEPTION}
};

/* The default policy. */
static const vm_policy_t default_policy;

/* A linked list of active VM policies. */
static vm_policy_t *policies;

static const char *
rule_name(vm_policy_type_t type)
{
  switch(type) {
  case VM_POLICY_TYPE_BANDWIDTH:
    return "BANDWIDTH";
  case VM_POLICY_TYPE_CPU:
    return "CPU";
  case VM_POLICY_TYPE_FILE:
    return "FILE";
  case VM_POLICY_TYPE_MEMORY:
    return "MEMORY";
  case VM_POLICY_TYPE_NET:
    return "NET";
  case VM_POLICY_TYPE_POWER:
    return "POWER";
  case VM_POLICY_TYPE_RESOURCES:
    return "RESOURCES";
  case VM_POLICY_TYPE_THREADS:
    return "THREADS";
  default:
    return NULL;
  }
}

static void
print_rule(vm_policy_rule_t *rule)
{
  const char *name;
#if 0
  char ipv6_address[50];
#endif

  name = rule_name(rule->type);
  if(name == NULL) {
    VM_DEBUG(VM_DEBUG_LOW,
            "Attempt to print a rule with an invalid type: %d",
	    rule->type);
    return;
  }

  VM_PRINTF("%s rule: ", name);
  switch(rule->type) {
  case VM_POLICY_TYPE_BANDWIDTH:
    VM_PRINTF("%u bits/s", rule->bandwidth.throughput);
    break;
  case VM_POLICY_TYPE_CPU:
    VM_PRINTF("usage %d%%, %s", rule->cpu.usage_percentage,
	   rule->cpu.window ? "sliding window" : "cumulative");
    break;
  case VM_POLICY_TYPE_FILE:
    VM_PRINTF("path %s, permission %d", rule->file.path, rule->file.flags);
    break;
  case VM_POLICY_TYPE_MEMORY:
    VM_PRINTF("max %u bytes", (unsigned)rule->memory.limit);
    break;
  case VM_POLICY_TYPE_NET:
#if 0
    if(inet_ntop(AF_INET6, &rule->net.address,
		 &ipv6_address, sizeof(ipv6_address)) == 1) {
      VM_PRINTF("address %s", ipv6_address);
    }
#endif
    break;
  case VM_POLICY_TYPE_POWER:
    VM_PRINTF("allocated power %d mW", rule->power.allocated_power);
    break;
  case VM_POLICY_TYPE_RESOURCES:
    if(rule->resources.resource_access & VM_POLICY_RESOURCE_SUPERUSER) {
      VM_PRINTF("superuser ");
    }
    if(rule->resources.resource_access & VM_POLICY_RESOURCE_CONSOLE) {
      VM_PRINTF("console ");
    }
    if(rule->resources.resource_access & VM_POLICY_RESOURCE_STATS) {
      VM_PRINTF("stats ");
    }
    if(rule->resources.resource_access & VM_POLICY_RESOURCE_IPC) {
      VM_PRINTF("ipc ");
    }
    break;
  case VM_POLICY_TYPE_THREADS:
    VM_PRINTF("max %u thread%s", (unsigned)rule->threads.limit,
	   rule->threads.limit == 1 ? "" : "s");
    break;
  default:
    return;
  }
  VM_PRINTF("\n");
}

static int
assert_unique_rule(vm_policy_t *policy, vm_policy_type_t type)
{
  const char *name;

  name = rule_name(type);
  if(name != NULL && policy->rules[type] != NULL) {
    VM_DEBUG(VM_DEBUG_LOW, "Cannot have multiple %s rules!", name);
    return 0;
  }

  return 1;
}

static vm_policy_t *
find_policy(const char *program_name)
{
  vm_policy_t *policy;

  for(policy = policies; policy != NULL; policy = policy->next) {
    if(strcmp(policy->program_name, program_name) == 0) {
      return policy;
    }
  }

  return NULL;
}

void
vm_policy_init_program(vm_program_t *program)
{
  static uint8_t generated_policies;
  int result;

  if(!generated_policies) {
    result = vm_policy_define();
    VM_DEBUG(VM_DEBUG_MEDIUM, "Generating the security policies... %s",
      result ? "OK" : "FAILED");
    generated_policies = 1;
    if(policies == NULL) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "No custom security policies were found");
    }
  }

  program->policy = find_policy(program->name);
  if(program->policy == NULL) {
    program->policy = &default_policy;
    VM_DEBUG(VM_DEBUG_LOW, "Policy for program \"%s\": *DEFAULT*",
             program->name);
  } else {
    VM_DEBUG(VM_DEBUG_LOW, "Policy for program \"%s\": *CUSTOM*",
             program->name);
  }

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  vm_policy_print(program->policy);
#endif

#if VM_SYSTRACE
  SET(program->flags, VM_PROGRAM_FLAG_SYSTRACE);
#endif
}

vm_policy_t *
vm_policy_add(const char *program_name, const void *hash, unsigned hash_length)
{
  vm_policy_t *policy;
  int i;

  if(find_policy(program_name)) {
    return NULL;
  }

  policy = VM_MALLOC(sizeof(vm_policy_t));
  if(policy == NULL) {
    return NULL;
  }

  policy->program_name = VM_STRDUP(program_name);
  if(policy->program_name == NULL) {
    VM_FREE(policy);
    return NULL;
  }

  for(i = 0; i < VM_POLICY_TYPE_COUNT; i++) {
    policy->rules[i] = NULL;
  }

  policy->next = policies;
  policies = policy;

  return policy;
}

int
vm_policy_add_rule(vm_policy_t *policy, vm_policy_rule_t *rule_arg)
{
  vm_policy_rule_t *rule;

  rule = VM_MALLOC(sizeof(vm_policy_rule_t));
  if(rule == NULL) {
    return 0;
  }

  rule->next = NULL;
  rule->type = rule_arg->type;

  switch(rule->type) {
  case VM_POLICY_TYPE_BANDWIDTH:
    memcpy(&rule->bandwidth, &rule_arg->bandwidth, sizeof(rule->bandwidth));
    if(!assert_unique_rule(policy, VM_POLICY_TYPE_BANDWIDTH)) {
      goto error;
    }
    break;
  case VM_POLICY_TYPE_CPU:
    memcpy(&rule->cpu, &rule_arg->cpu, sizeof(rule->cpu));
    if(!assert_unique_rule(policy, VM_POLICY_TYPE_CPU)) {
      goto error;
    }
    break;
  case VM_POLICY_TYPE_FILE:
    rule->file.path = strdup(rule_arg->file.path);
    rule->file.flags = rule_arg->file.flags;
    rule->next = policy->rules[VM_POLICY_TYPE_FILE];
    break;
  case VM_POLICY_TYPE_MEMORY:
    memcpy(&rule->memory, &rule_arg->memory, sizeof(rule->memory));
    if(!assert_unique_rule(policy, VM_POLICY_TYPE_MEMORY)) {
      goto error;
    }
    break;
  case VM_POLICY_TYPE_NET:
    memset(&rule->net, 0, sizeof(rule->net));
    rule->net.address = rule_arg->net.address;
    rule->net.port = rule_arg->net.port;
    rule->next = policy->rules[VM_POLICY_TYPE_NET];
    break;
  case VM_POLICY_TYPE_POWER:
    memcpy(&rule->power, &rule_arg->power, sizeof(rule->power));
    if(!assert_unique_rule(policy, VM_POLICY_TYPE_POWER)) {
      goto error;
    }
    break;
  case VM_POLICY_TYPE_RESOURCES:
    memcpy(&rule->resources, &rule_arg->resources, sizeof(rule->resources));
    if(!assert_unique_rule(policy, VM_POLICY_TYPE_RESOURCES)) {
      goto error;
    }
    break;
  case VM_POLICY_TYPE_THREADS:
    memcpy(&rule->threads, &rule_arg->threads, sizeof(rule->threads));
    if(!assert_unique_rule(policy, VM_POLICY_TYPE_THREADS)) {
      goto error;
    }
    break;
  default:
    VM_DEBUG(VM_DEBUG_LOW, "Invalid rule type: %d", rule->type);
    goto error;
  }

  rule->reaction = default_reaction_rules[rule->type].reaction;
  policy->rules[rule->type] = rule;

  return 1;
error:
  VM_FREE(rule);
  return 0;
}

void
vm_policy_print(const vm_policy_t *policy)
{
  int i;
  vm_policy_rule_t *rule;

  for(i = 0; i < VM_POLICY_TYPE_COUNT; i++) {
    for(rule = policy->rules[i]; rule != NULL; rule = rule->next) {
      print_rule(rule);
    }
  }
}

vm_policy_reaction_t
vm_policy_default_reaction(vm_policy_type_t type)
{
  return default_reaction_rules[type].reaction;
}
