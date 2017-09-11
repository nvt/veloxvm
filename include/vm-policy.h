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

#ifndef VM_POLICY_H
#define VM_POLICY_H

#include <inttypes.h>

typedef enum vm_policy_reaction {
  VM_POLICY_REACTION_EXCEPTION = 0,
  VM_POLICY_REACTION_REPORT    = 1,
  VM_POLICY_REACTION_SLOWDOWN  = 2,
  VM_POLICY_REACTION_KILL      = 3
} vm_policy_reaction_t;

typedef enum vm_policy_type {
  VM_POLICY_TYPE_BANDWIDTH = 0,
  VM_POLICY_TYPE_CPU       = 1,
  VM_POLICY_TYPE_FILE      = 2,
  VM_POLICY_TYPE_MEMORY    = 3,
  VM_POLICY_TYPE_NET       = 4,
  VM_POLICY_TYPE_POWER     = 5,
  VM_POLICY_TYPE_RESOURCES = 6,
  VM_POLICY_TYPE_THREADS   = 7
} vm_policy_type_t;

typedef enum vm_policy_unit {
  VM_POLICY_UNIT_BPS       = 0, /* Bits per second. */
  VM_POLICY_UNIT_KBPS      = 1, /* Kilobits per second. */
  VM_POLICY_UNIT_UW        = 2, /* Microwatts. */
  VM_POLICY_UNIT_MW        = 3, /* Milliwatts. */
  VM_POLICY_UNIT_KB        = 4, /* Kilobytes. */
  VM_POLICY_UNIT_MB        = 5  /* Megabytes. */
} vm_policy_unit_t;

#define VM_POLICY_REACTION_COUNT (VM_POLICY_REACTION_KILL + 1)
#define VM_POLICY_TYPE_COUNT     (VM_POLICY_TYPE_THREADS + 1)

#define VM_POLICY_RESOURCE_SUPERUSER 0x01
#define VM_POLICY_RESOURCE_CONSOLE   0x02
#define VM_POLICY_RESOURCE_DNS       0x04
#define VM_POLICY_RESOURCE_IPC       0x08
#define VM_POLICY_RESOURCE_STATS     0x10

struct vm_policy_bandwidth {
  uint64_t throughput;
  vm_policy_unit_t unit;
};

struct vm_policy_cpu {
  uint8_t usage_percentage;
  uint8_t window;
};

struct vm_policy_file {
  const char *path;
  uint8_t flags;
};

struct vm_policy_memory {
  uint16_t limit;
  vm_policy_unit_t unit;
};

struct vm_policy_net {
  const unsigned char *address;
  uint8_t protocol;
  uint16_t port;
};

struct vm_policy_power {
  uint16_t allocated_power;
  vm_policy_unit_t unit;
};

struct vm_policy_resources {
  uint8_t resource_access;
};

struct vm_policy_threads {
  uint8_t limit;
};

typedef struct vm_policy_rule {
  struct vm_policy_rule *next;
  vm_policy_type_t type;
  vm_policy_reaction_t reaction;
  union {
    struct vm_policy_bandwidth bandwidth;
    struct vm_policy_cpu cpu;
    struct vm_policy_file file;
    struct vm_policy_memory memory;
    struct vm_policy_net net;
    struct vm_policy_power power;
    struct vm_policy_resources resources;
    struct vm_policy_threads threads;
  };
} vm_policy_rule_t;

typedef struct vm_policy {
  struct vm_policy *next;
  const char *program_name;
  vm_policy_rule_t *rules[VM_POLICY_TYPE_COUNT];
} vm_policy_t;

#endif /* VM_POLICY_H */
