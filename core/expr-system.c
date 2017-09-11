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

#include "vm-functions.h"
#include "vm-lib.h"
#include "vm-log.h"
#include "vm-native.h"

#ifndef VM_RANDOM_FUNCTION
#define VM_RANDOM_FUNCTION random
#endif

VM_FUNCTION(system_info)
{
  char vm_version[80];
  vm_vector_t *result;

  snprintf(vm_version, sizeof(vm_version), "%s %d.%d (%s)",
         VM_NAME, VM_VERSION_MAJOR, VM_VERSION_MINOR, VM_MAKE_STRING(VM_PORT));

  result = vm_vector_create(&thread->result, 2, VM_VECTOR_FLAG_REGULAR);
  if(result == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  if(vm_string_create(&result->elements[0], -1, vm_version) == NULL ||
     vm_string_create(&result->elements[1], -1, vm_native_get_os_version()) == NULL) {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
  }
}

VM_FUNCTION(load_program)
{
#ifdef BUNDLE
  /* Cannot load external programs when the VM and the app are bundled. */
  vm_signal_error(thread, VM_ERROR_POLICY);
  return;
#endif

  if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_SUPERUSER)) {
    return;
  }

  if(vm_load_program(argv[0].value.string->str) == 0) {
    vm_signal_error(thread, VM_ERROR_IO);
  }
}

VM_FUNCTION(import)
{
  if(vm_lib_load(thread->program, argv[0].value.string->str) == 0) {
    vm_signal_error(thread, VM_ERROR_LIBRARY);
  }

  VM_PUSH_BOOLEAN(VM_TRUE);
}

VM_FUNCTION(get_devices)
{
  vm_device_t *devices;
  vm_device_t *device;
  int device_count;
  vm_vector_t *result;

  devices = vm_device_get_all();

  /* Count the device in order to create a vector. */
  for(device_count = 0, device = devices;
      device != NULL;
      device_count++, device = device->next) {
  }

  result = vm_vector_create(&thread->result, device_count,
                            VM_VECTOR_FLAG_REGULAR);
  if(result == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(device_count = 0, device = devices;
      device != NULL;
      device = device->next) {
    if(vm_string_create(&result->elements[device_count++], -1, device->name) == NULL) {
      vm_signal_error(thread, VM_ERROR_INTERNAL);
    }
  }
}

VM_FUNCTION(print)
{
  vm_integer_t i;

  for(i = 0; i < argc; i++) {
    vm_write_object(NULL, &argv[i]);
  }
}

VM_FUNCTION(random)
{
  vm_integer_t modulo;

  modulo = argc == 1 ? argv[0].value.integer : VM_INTEGER_MAX;
  if(modulo == 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
  } else {
    VM_PUSH_INTEGER(VM_RANDOM_FUNCTION() % modulo);
  }
}

VM_FUNCTION(time)
{
  vm_time_t current_time;

  if(vm_native_time(&current_time)) {
    VM_PUSH_INTEGER(current_time.sec * 1000 + current_time.msec);
  }
}

VM_FUNCTION(get_programs)
{
  vm_program_t *programs;
  vm_program_t *program;
  int program_count;
  vm_vector_t *result;

  if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_SUPERUSER)) {
    return;
  }

  programs = vm_get_programs();

  /* Count the device in order to create a vector. */
  for(program_count = 0, program = programs;
      program != NULL;
      program_count++, program = program->next) {
  }

  result = vm_vector_create(&thread->result, program_count,
                            VM_VECTOR_FLAG_REGULAR);
  if(result == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(program_count = 0, program = programs;
      program != NULL;
      program = program->next) {
    if(vm_string_create(&result->elements[program_count++], -1, program->name) == NULL) {
      vm_signal_error(thread, VM_ERROR_INTERNAL);
    }
  }
}

VM_FUNCTION(program_info)
{
  const char *name;
  vm_program_t *program;

  if(argc == 0) {
    if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_STATS)) {
      return;
    }

    name = thread->program->name;
  } else {
    if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_SUPERUSER)) {
      return;
    }

    name = argv[0].value.string->str;
  }

  program = vm_find_program(name);
  if(program == NULL) {
    vm_signal_error(thread, VM_ERROR_NAME);
    return;
  }

  VM_PRINTF("Retrieving information for program \"%s\"\n", name);
  VM_PRINTF("Expressions: %u (%u bytes)\n",
	    (unsigned)VM_TABLE_SIZE(program->exprv),
	    (unsigned)program->exprv.size);
  VM_PRINTF("Strings: %u (%u bytes)\n",
	    (unsigned)VM_TABLE_SIZE(program->strings),
	    (unsigned)program->strings.size);
  VM_PRINTF("Symbols: %u (%u bytes)\n",
	    (unsigned)VM_TABLE_SIZE(program->symbols),
	    (unsigned)program->symbols.size);
  VM_PRINTF("Static program size: %u bytes\n",
	    (unsigned)(sizeof(vm_program_t) + program->exprv.size +
		       program->strings.size + program->symbols.size));
  VM_PRINTF("Running threads: %u\n", (unsigned)program->nthreads);
}

VM_FUNCTION(exit)
{
  VM_DEBUG(VM_DEBUG_LOW, "Explicit exit call");
  thread->status = VM_THREAD_EXITING;
}
