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

#ifndef VM_H
#define VM_H

#include <stdint.h>
#include <string.h>

#include "vm-bytecode.h"
#include "vm-config.h"
#include "vm-id.h"
#include "vm-macros.h"
#include "vm-objects.h"
#include "vm-perf-attr.h"
#include "vm-port.h"
#include "vm-policy.h"
#include "vm-table.h"
#include "vm-types.h"

#define VM_NAME "VeloxVM"
#define VM_VERSION_MAJOR 1
#define VM_VERSION_MINOR 0
#define VM_COPYRIGHT "Copyright 2012-2017 RISE SICS AB"

struct vm_thread;
typedef void (*vm_operator_t)(struct vm_thread *, vm_type_set_t,
			      vm_integer_t, vm_obj_t *);

#define VM_PROCEDURE_EVAL_ARGS   0x01

typedef struct vm_procedure {
  vm_operator_t operator;
  vm_expr_id_t expr_id;
  vm_type_set_t valid_types;
  int8_t min_args;
  int8_t max_args;
  uint8_t flags;
} vm_procedure_t;

typedef struct vm_symbol {
  vm_obj_t obj;
  const char *name;
} vm_symbol_t;

typedef struct vm_symbol_bind {
  vm_obj_t obj;
  vm_symbol_id_t symbol_id;
} vm_symbol_bind_t;

#define VM_PROGRAM_FLAG_PRIVILEGED        0x1
#define VM_PROGRAM_FLAG_SYSTRACE          0x2
#define VM_PROGRAM_FLAG_STRICT_ASSIGNMENT 0x4
#define VM_PROGRAM_FLAG_SLOW_DOWN         0x8

typedef struct vm_program {
  vm_table_t strings;
  vm_table_t symbols;
  vm_table_t exprv;
  struct vm_program *next;
  const vm_policy_t *policy;
  char *name;
  vm_obj_t *symbol_bindings;
#if VM_INSTRUCTION_PROFILING
  unsigned long *exec_count;
#endif
  vm_perf_attr_t perf_attr;
  vm_integer_t program_id;
  uint8_t nthreads;
  uint8_t flags;
} vm_program_t;

#define VM_EXPR_HAVE_OBJECTS 0x01
#define VM_EXPR_LAMBDA       0x02
#define VM_EXPR_TAIL_CALL    0x04
#define VM_EXPR_RESTART      0x08
#define VM_EXPR_SAVE_FRAME   0x10
#define VM_EXPR_CONTINUATION 0x20

typedef uint32_t vm_arg_set_t;

typedef struct vm_expr {
  vm_obj_t argv[VM_OBJECT_STACK_SIZE];
  vm_arg_set_t eval_requested;
  vm_arg_set_t eval_completed;
  vm_ip_t ip;
  vm_ip_t end;
  const vm_procedure_t *procedure;
  vm_symbol_bind_t *bindv;
  vm_expr_id_t expr_id;
  uint8_t flags;
  uint8_t argc;
  uint8_t bindc;
  uint8_t eval_arg;
} vm_expr_t;

typedef enum {
  VM_ERROR_INTERNAL            =  0,
  VM_ERROR_HEAP                =  1,
  VM_ERROR_BYTECODE            =  2,
  VM_ERROR_TOKEN               =  3,
  VM_ERROR_ARGUMENT_COUNT      =  4,
  VM_ERROR_ARGUMENT_TYPES      =  5,
  VM_ERROR_ARGUMENT_VALUE      =  6,
  VM_ERROR_EXPR_ID             =  7,
  VM_ERROR_STRING_ID           =  8,
  VM_ERROR_SYMBOL_ID           =  9,
  VM_ERROR_SYMBOL_UNDEFINED    = 10,
  VM_ERROR_THREAD              = 11,
  VM_ERROR_STACK_OVERFLOW      = 12,
  VM_ERROR_STACK_UNDERFLOW     = 13,
  VM_ERROR_DIV0                = 14,
  VM_ERROR_UNIMPLEMENTED       = 15,
  VM_ERROR_WRITE_PROHIBITED    = 16,
  VM_ERROR_IO                  = 17,
  VM_ERROR_PORT_CLOSED         = 18,
  VM_ERROR_SOCKET              = 19,
  VM_ERROR_POLICY              = 20,
  VM_ERROR_NAME                = 21,
  VM_ERROR_UNHANDLED_EXCEPTION = 22,
  VM_ERROR_LIBRARY             = 23,
  VM_ERROR_SYSCALL             = 24
} vm_error_type_t;

typedef struct vm_error {
  vm_obj_t error_obj;
  vm_ip_t error_ip;
  vm_error_type_t error_type;
#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  const char *file;
  unsigned line;
#endif
} vm_error_t;

typedef struct vm_thread {
  vm_expr_t *exprv[VM_CONTEXT_STACK_SIZE];
  vm_obj_t result;
  vm_obj_t specific_obj;
  vm_expr_t *expr;
  vm_program_t *program;
  vm_error_t error;
  vm_thread_stats_t stats;
  vm_thread_status_t status;
  vm_id_t id;
  uint8_t exprc;
} vm_thread_t;

typedef struct vm_port_io {
  int (*open)(vm_port_t *, const char *, uint8_t);
  int (*read)(vm_port_t *, char *buf, size_t);
  int (*read_object)(vm_port_t *, vm_obj_t *);
  int (*write)(vm_port_t *, const char *, size_t);
  void (*close)(vm_port_t *);
} vm_port_io_t;

typedef struct vm_device {
  struct vm_device *next;
  const char *name;
  const vm_port_io_t *io;
  uint8_t flags;
} vm_device_t;

typedef struct vm_memory_stats {
  uint64_t allocations;
  uint64_t allocated_bytes;
  uint64_t manual_deallocations;
  uint64_t gc_deallocations;
  uint64_t mempool_forwards;
  uint64_t gc_invocations;
} vm_memory_stats_t;

/* Scheduler functions. (vm-sched.c) */
void vm_thread_set_expr(vm_thread_t *, vm_expr_id_t);
vm_thread_t *vm_current_thread(void);
vm_result_t vm_run(void);

/* Program loader functions. (vm-loader.c) */
int vm_load_program(const char *);
int vm_unload_program(vm_program_t *);
vm_program_t *vm_find_program(const char *);
vm_program_t *vm_get_programs(void);

/* Main functions. (vm-main.c) */
void vm_print_error(vm_thread_t *);
void vm_print_debug(unsigned level, const char *, ...);
int vm_init(void);
void vm_exit(void);

/* Symbol functions. (vm-symbols.c) */
void vm_symbol_bind(vm_thread_t *, vm_symbol_ref_t *, vm_obj_t *);
const char *vm_symbol_lookup(vm_program_t *, vm_symbol_ref_t *);
vm_obj_t *vm_symbol_resolve(vm_thread_t *, vm_symbol_ref_t *);
int vm_symbol_get_ref(vm_thread_t *, const char *, vm_symbol_ref_t *);

/* Language-specific functions. (vm-lang.c) */
unsigned vm_procedure_count(void);
const vm_procedure_t *vm_procedure_lookup(vm_program_t *, vm_symbol_ref_t *);
void vm_eval_object(vm_thread_t *, vm_obj_t *);
void vm_get_object(vm_thread_t *, vm_obj_t *);
void vm_write_object(vm_port_t *, vm_obj_t *);
int vm_is_procedure(vm_thread_t *, vm_obj_t *);
#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
void _vm_signal_error(vm_thread_t *, vm_error_type_t, const char *, const unsigned);
#define vm_signal_error(thread, error) \
          _vm_signal_error((thread), (error), __FILE__, __LINE__)
#else
void _vm_signal_error(vm_thread_t *, vm_error_type_t);
#define vm_signal_error(thread, error) \
          _vm_signal_error((thread), (error))
#endif /* VM_DEBUG */
void vm_set_error_object(vm_thread_t *, vm_obj_t *);
void vm_set_error_string(vm_thread_t *, const char *);
void vm_raise_exception(vm_thread_t *, vm_obj_t *);
void vm_return_from_function(vm_thread_t *, vm_obj_t *);
void vm_eval_expr(vm_thread_t *, vm_expr_t *);
void vm_print_stack_trace(vm_thread_t *);
void vm_print_eval_expr(vm_thread_t *);
void vm_print_eval_object(vm_thread_t *, vm_obj_t *);
int vm_objects_equal(vm_thread_t *thread, vm_obj_t *, vm_obj_t *);
int vm_objects_deep_equal(vm_thread_t *thread, vm_obj_t *, vm_obj_t *);
int vm_object_deep_copy(vm_obj_t *, vm_obj_t *);

/* Memory functions. (vm-memory.c) */
void *vm_alloc(unsigned);
void vm_free(void *);
void vm_free_all(void);
void vm_gc(void);
void vm_memory_get_stats(vm_memory_stats_t *);
int vm_memory_init(void);

/* Thread functions. (vm-thread.c) */
int vm_thread_init(void);
void thread_obj_create(vm_obj_t *, vm_thread_t *);
vm_thread_t *vm_thread_create(vm_program_t *);
void vm_thread_destroy(vm_thread_t *);
vm_thread_t *vm_thread_spawn(vm_thread_t *, vm_obj_t *);
vm_thread_t *vm_thread_fork(vm_thread_t *);
int vm_thread_kill(vm_id_t);
vm_thread_t *vm_thread_get(vm_id_t);
vm_thread_t *vm_thread_get_by_index(unsigned);
vm_id_index_t vm_thread_get_index(vm_thread_t *);
unsigned vm_thread_running(void);
void vm_thread_print(vm_thread_t *);
void vm_thread_print_form(vm_thread_t *, vm_expr_t *);
void vm_thread_print_ip(vm_thread_t *, vm_expr_t *);

/* Thread stack functions. (vm-thread-stack.c) */
int vm_thread_stack_create(void);
void vm_thread_stack_destroy(void);
vm_expr_t *vm_thread_stack_push(vm_thread_t *);
void vm_thread_stack_pop(vm_thread_t *);
vm_expr_t *vm_thread_stack_alloc(vm_thread_t *);
void vm_thread_stack_free(vm_expr_t *);
void vm_thread_stack_print_frame(vm_thread_t *, vm_expr_t *);
int vm_thread_stack_copy(vm_thread_t *, vm_thread_t *);

/* Object utilities. (vm-objects.c) */
void vm_ext_type_register(vm_ext_type_t *);
vm_string_t *vm_string_create(vm_obj_t *, vm_integer_t, const char *);
char *vm_string_resolve(vm_thread_t *, vm_string_t *);
vm_vector_t *vm_vector_create(vm_obj_t *, vm_integer_t, vm_vector_flags_t);
int vm_vector_set(vm_obj_t *, vm_integer_t, vm_obj_t *);

/* Object interpretation from string input. (vm-object-interpret.c) */
vm_boolean_t vm_object_interpret(vm_obj_t *, const char *);

/* Device management. (vm-device.c) */
int vm_device_register(const char *, const vm_port_io_t *, uint8_t flags);
vm_device_t *vm_device_lookup(const char *);
vm_device_t *vm_device_get_all(void);

/* VM policy definition function. (vm-policy-defs-{custom,default}.c) */
int vm_policy_define(void);

/* VM policy specification functions. (vm-policy-spec.c) */
void vm_policy_init_program(vm_program_t *);
vm_policy_t *vm_policy_add(const char *, const void *, unsigned);
int vm_policy_add_rule(vm_policy_t *, vm_policy_rule_t *);
void vm_policy_print(const vm_policy_t *);
vm_policy_reaction_t vm_policy_default_reaction(vm_policy_type_t);

/* VM policy enforcement functions. (vm-policy-enforce.c) */
vm_boolean_t vm_policy_check_bandwidth(vm_thread_t *);
vm_boolean_t vm_policy_check_cpu(vm_program_t *, unsigned);
vm_boolean_t vm_policy_check_file(vm_thread_t *, const char *, unsigned);
vm_boolean_t vm_policy_check_net(vm_thread_t *, const vm_vector_t *, unsigned);
vm_boolean_t vm_policy_check_power(vm_thread_t *);
vm_boolean_t vm_policy_check_resources(vm_thread_t *, unsigned);
vm_boolean_t vm_policy_check_threads(vm_thread_t *);

#endif /* !VM_H */
