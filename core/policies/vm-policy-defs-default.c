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
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include "vm.h"
#include "vm-policy.h"

/*
 * Permissive fallback policy. Attached to every program that does not
 * have a named policy of its own, so that VM_SUPERUSER_MODE=0 can be the
 * default without breaking existing apps and tests. Tighten or replace
 * this file (or define a vm-policy-defs-custom.c, which the Makefile
 * prefers over the default) to enforce a real policy.
 */
int
vm_policy_define(void)
{
  vm_policy_rule_t rule;

  rule = (vm_policy_rule_t){
    .type = VM_POLICY_TYPE_RESOURCES,
    .resources.resource_access = VM_POLICY_RESOURCE_SUPERUSER,
  };
  if(!vm_policy_add_rule(&vm_policy_default, &rule)) {
    return 0;
  }

  rule = (vm_policy_rule_t){
    .type = VM_POLICY_TYPE_THREADS,
    .threads.limit = 0xff,
  };
  if(!vm_policy_add_rule(&vm_policy_default, &rule)) {
    return 0;
  }

  rule = (vm_policy_rule_t){
    .type = VM_POLICY_TYPE_CPU,
    .cpu.usage_percentage = 100,
  };
  if(!vm_policy_add_rule(&vm_policy_default, &rule)) {
    return 0;
  }

  rule = (vm_policy_rule_t){
    .type = VM_POLICY_TYPE_FILE,
    .file.path = "*",
  };
  if(!vm_policy_add_rule(&vm_policy_default, &rule)) {
    return 0;
  }

  rule = (vm_policy_rule_t){
    .type = VM_POLICY_TYPE_NET,
    .net.address = NULL,
    .net.port = 0,
  };
  if(!vm_policy_add_rule(&vm_policy_default, &rule)) {
    return 0;
  }

  return 1;
}
