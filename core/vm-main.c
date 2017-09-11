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

#include <stdarg.h>
#include <stdio.h>

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"
#include "vm-profiler.h"

#if VM_ERROR_MESSAGES
static const char *error_messages[] = {
  "Internal error",
  "Heap exhausted",
  "Invalid bytecode",
  "Invalid token",
  "Argument count",
  "Argument types",
  "Argument value",
  "Invalid expr ID",
  "Invalid string ID",
  "Invalid symbol ID",
  "Undefined symbol",
  "Thread error",
  "Stack overflow",
  "Stack underflow",
  "Division by zero",
  "Unimplemented function",
  "Write attempt on immutable object",
  "I/O error",
  "I/O call on closed port",
  "Socket error",
  "Policy violation",
  "Named object does not exist",
  "Unhandled exception",
  "Unable to load a library",
  "System call error"
};

static const char *
error_message(vm_error_type_t error_type)
{
  if(error_type >= ARRAY_SIZE(error_messages)) {
    return "unknown error";
  }
  return error_messages[error_type];
}
#endif /* VM_ERROR_MESSAGES */

void
vm_print_debug(unsigned level, const char *format, ...)
{
  va_list ap;
  char buf[VM_CONSOLE_BUFFER_SIZE];
#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  vm_time_t current_time;
  vm_thread_t *thread;
#endif

  if(level > VM_DEBUG_LEVEL) {
    return;
  }

  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  vm_native_time(&current_time);

  VM_PRINTF("VM Debug(%u): t=%lu.%03u ", level,
            (unsigned long)current_time.sec, (unsigned)current_time.msec);

  thread = vm_current_thread();
  if(thread != NULL) {
    VM_PRINTF("program=\"%s\" thread=%ld ",
              thread->program->name == NULL ? "<null>" : thread->program->name,
              (long)thread->id);
  }

  VM_PRINTF("msg=\"%s\"\n", buf);
#else
  VM_PRINTF("VM: %s\n", buf);
#endif
}

void
vm_print_error(vm_thread_t *thread)
{
  VM_PRINTF("[%s:%u] Error type %u at IP ", thread->program->name,
            thread->id, thread->error.error_type);
  thread->expr->ip = thread->error.error_ip;
  vm_thread_print_ip(thread, thread->expr);

#if VM_ERROR_MESSAGES
  if(thread->error.error_type == VM_ERROR_SYMBOL_UNDEFINED) {
    VM_PRINTF(": %s", error_message(thread->error.error_type));
  } else {
    VM_PRINTF(": %s", error_message(thread->error.error_type));
  }

  if(thread->error.error_obj.type != VM_TYPE_NONE) {
    VM_PRINTF(": ");
    vm_write_object(NULL, &thread->error.error_obj);
  } else {
    VM_PRINTF(".");
  }
#else
  VM_PRINTF(".");
#endif /* VM_ERROR_MESSAGES */

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
  VM_PRINTF(" (Source: %s:%u)", thread->error.file, thread->error.line);
#endif

  VM_PRINTF("\n");

  vm_print_stack_trace(thread);
}

int
vm_init(void)
{
  if(VM_DEBUG_LEVEL >= VM_DEBUG_LOW) {
    VM_PRINTF("%s %d.%d (%s)\n%s\n",
              VM_NAME, VM_VERSION_MAJOR, VM_VERSION_MINOR,
              VM_MAKE_STRING(VM_PORT), VM_COPYRIGHT);
  }

  if(vm_memory_init() == 0 ||
     vm_native_init() == 0 ||
     vm_thread_init() == 0 ||
     vm_thread_stack_create() == 0 ||
#if VM_PROFILER_ENABLE
     vm_profiler_init() == 0 ||
#endif
     vm_lib_init() == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Initialization failed");
    return 0;
  }

  if(VM_SUPERUSER_MODE) {
    VM_DEBUG(VM_DEBUG_LOW, "Application policies are disabled");
  }

  return 1;
}

void
vm_exit(void)
{
#if VM_PROFILER_ENABLE
  vm_profiler_print_stats();
#endif

  vm_unload_program(NULL);

#if VM_DEALLOCATE_AT_EXIT
  vm_free_all();
  vm_thread_stack_destroy();
#endif

  VM_DEBUG(VM_DEBUG_LOW, "Exiting the VM");
}
