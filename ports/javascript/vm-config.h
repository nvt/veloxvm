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

#ifndef VM_CONFIG_H
#define VM_CONFIG_H

/*
 * Debug level.
 *
 * 0: show no debug messages.
 * 1: show the most important debug messages.
 * 5: show verbose debug messages.
 * 10: show all debug messages.
 */
#ifndef VM_DEBUG_LEVEL
#define VM_DEBUG_LEVEL 1
#endif

/* Superuser mode entails that the VM runs programs without
   enforcing any security policies. */
#ifndef VM_SUPERUSER_MODE
#define VM_SUPERUSER_MODE 1
#endif

/* This setting determined whether to keep the VM running even if there
   are no loaded programs. */
#ifndef VM_ALWAYS_ON
#define VM_ALWAYS_ON 0
#endif

/* Enables an LWM2M-based control interface where VM
   administrators can connect during runtime. */
#ifndef VM_SERVER
#define VM_SERVER 0
#endif

/* Internal VM profiling that measures the performance of core operations. */
#ifndef VM_PROFILER_ENABLE
#define VM_PROFILER_ENABLE 0
#endif

/*
 * Determine whether instruction profiling should be enabled.
 * This can be used to gain insight into the performance bottlenecks
 * of VM programs , but adds to the memory footprint of each program.
 */
#ifndef VM_INSTRUCTION_PROFILING
#define VM_INSTRUCTION_PROFILING 0
#endif

#ifndef VM_PORT_AMOUNT
#define VM_PORT_AMOUNT 100
#endif

/* The time in milliseconds to make a thread sleep when polling a condition. */
#ifndef VM_POLL_TIME
#define VM_POLL_TIME 10
#endif

#ifndef VM_HEAP_SIZE
#define VM_HEAP_SIZE 100000
#endif

#ifndef VM_OBJECT_POOL_SIZE
#define VM_OBJECT_POOL_SIZE 100000
#endif

#ifndef VM_FRAME_POOL_SIZE
#define VM_FRAME_POOL_SIZE 100000
#endif

#ifndef VM_CONSOLE_BUFFER_SIZE
#define VM_CONSOLE_BUFFER_SIZE 1024
#endif

#ifndef VM_GC_AGGRESSIVE
#define VM_GC_AGGRESSIVE 1
#endif

#ifndef VM_GC_MIN_ALLOCATED
#define VM_GC_MIN_ALLOCATED (VM_HEAP_SIZE / 2)
#endif

/* Deallocation of dynamic memory when exiting is only needed in host
   environments that do not do so automatically for system processes. */
#ifndef VM_DEALLOCATE_AT_EXIT
#define VM_DEALLOCATE_AT_EXIT 0
#endif

#ifndef VM_ENABLE_REALS
#define VM_ENABLE_REALS 1
#endif

#ifndef VM_EXEC_INSTR_PER_INVOCATION
#define VM_EXEC_INSTR_PER_INVOCATION 1000
#endif

#ifndef VM_THREAD_AMOUNT
#define VM_THREAD_AMOUNT 10
#endif

#ifndef VM_SCHEDULE_TIMEOUT
#define VM_SCHEDULE_TIMEOUT 2
#endif

#ifndef VM_CONTEXT_STACK_SIZE
#define VM_CONTEXT_STACK_SIZE 64
#endif

#ifndef VM_OBJECT_STACK_SIZE
#define VM_OBJECT_STACK_SIZE 16
#endif

#ifndef VM_ERROR_MESSAGES
#define VM_ERROR_MESSAGES 1
#endif

#ifndef VM_LIST_PRINT_LIMIT
#define VM_LIST_PRINT_LIMIT 10
#endif

#endif /* !VM_CONFIG_H */
