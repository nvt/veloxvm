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
#define VM_ALWAYS_ON 1
#endif

/* The remote administration server opens a TCP server port where VM
   administrators can connect to control the VM during runtime. */
#ifndef VM_SERVER
#define VM_SERVER 0
#endif

/* Internal VM profiling that measures the performance of core operations. */
#ifndef VM_PROFILER_ENABLE
#define VM_PROFILER_ENABLE 0
#endif

/* Emit periodic memory-pool utilisation lines from the performance monitor.
   Counters read are maintained on the hot path anyway, so the only cost is
   a few extra prints per monitor tick. */
#ifndef VM_MEMORY_PROFILING
#define VM_MEMORY_PROFILING 0
#endif

/* Force a full GC before each profile print so "used" figures exclude
   uncollected garbage. Defaults off on Contiki-NG because the perfmon
   fires periodically at runtime; enabling it adds a mark+sweep on every
   tick and can perceptibly shift program timing. Use the cumulative
   counters and the peak high-water marks if you need accuracy without
   the perturbation. */
#ifndef VM_MEMORY_PROFILING_GC
#define VM_MEMORY_PROFILING_GC 0
#endif

/* Tag every pool allocation with a category enum (cons cell, string
   header, vector elements, etc.) and emit a per-category histogram
   from the memory profiler. Adds 1 byte per pool slot plus a small
   fixed alloc-time cost. Off by default; enable in addition to
   VM_MEMORY_PROFILING for attribution data. */
#ifndef VM_ATTRIBUTION_ENABLE
#define VM_ATTRIBUTION_ENABLE 0
#endif

/*
 * Determine whether instruction profiling should be enabled.
 * This can be used to gain insight into the performance bottlenecks
 * of VM programs , but adds to the memory footprint of each program.
 */
#ifndef VM_INSTRUCTION_PROFILING
#define VM_INSTRUCTION_PROFILING 0
#endif

/* Maximum size of a single table item when loading bytecode.
   Backs a stack-allocated buffer in vm-loader.c read_table(), so on
   embedded targets this stays small to avoid blowing the thread
   stack. Native is hosted, so the loader can take any item that fits
   in a uint16_t length prefix. */
#ifndef VM_TABLE_MAX_ITEM_SIZE
#if CONTIKI_TARGET_NATIVE
#define VM_TABLE_MAX_ITEM_SIZE 65535
#else
#define VM_TABLE_MAX_ITEM_SIZE 255
#endif
#endif

/* Maximum number of I/O ports that can be in use simultaneously. */
#ifndef VM_PORT_AMOUNT
#if CONTIKI_TARGET_NATIVE
#define VM_PORT_AMOUNT 100
#else
#define VM_PORT_AMOUNT 20
#endif
#endif

/* The time in milliseconds to make a thread sleep when polling a condition. */
#ifndef VM_POLL_TIME
#define VM_POLL_TIME 100
#endif

/*
 * Default VM memory sizes, in bytes.
 *
 * Three tiers:
 *   - Zoul: tight legacy footprint (~12 kB total) for the original
 *     32 kB-RAM target.
 *   - Native: hosted Linux/macOS process; generous so the GC and
 *     frame pool do not become the bottleneck for non-trivial test
 *     programs. Smaller than the POSIX port so the GC's allocation
 *     hash table (sized by VM_HEAP_SIZE / sizeof(vm_list_item_t))
 *     does not balloon.
 *   - Other targets: a middle tier sized for modern IoT MCUs with
 *     ~256 kB RAM (nRF52840, cc1352, etc.) — the historic
 *     non-Zoul defaults.
 *
 * Each knob remains individually overridable.
 */
#ifndef VM_HEAP_SIZE
#if CONTIKI_TARGET_ZOUL
#define VM_HEAP_SIZE 5000
#elif CONTIKI_TARGET_NATIVE
#define VM_HEAP_SIZE 1048576
#else
#define VM_HEAP_SIZE 32768
#endif
#endif

#ifndef VM_OBJECT_POOL_SIZE
#if CONTIKI_TARGET_ZOUL
#define VM_OBJECT_POOL_SIZE 3000
#elif CONTIKI_TARGET_NATIVE
#define VM_OBJECT_POOL_SIZE 1048576
#else
#define VM_OBJECT_POOL_SIZE 16384
#endif
#endif

#ifndef VM_FRAME_POOL_SIZE
#if CONTIKI_TARGET_ZOUL
#define VM_FRAME_POOL_SIZE 4000
#elif CONTIKI_TARGET_NATIVE
#define VM_FRAME_POOL_SIZE 65536
#else
#define VM_FRAME_POOL_SIZE 8192
#endif
#endif

/* Size of the heapmem zone dedicated to the VM. All VM_MALLOC / VM_FREE /
   VM_REALLOC traffic flows through this zone, so it must be large enough
   to cover the Scheme heap, object pool, and frame pool plus any slack
   for fragmentation. It has no effect on the rest of the system. */
#ifndef VM_ZONE_SIZE
#define VM_ZONE_SIZE (VM_HEAP_SIZE + VM_OBJECT_POOL_SIZE + VM_FRAME_POOL_SIZE)
#endif

#ifndef VM_CONSOLE_BUFFER_SIZE
#define VM_CONSOLE_BUFFER_SIZE 80
#endif

#ifndef VM_GC_AGGRESSIVE
#define VM_GC_AGGRESSIVE 0
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
#define VM_ENABLE_REALS 0
#endif

#ifndef VM_EXEC_INSTR_PER_INVOCATION
#define VM_EXEC_INSTR_PER_INVOCATION 100
#endif

#ifndef VM_THREAD_AMOUNT
#define VM_THREAD_AMOUNT 20
#endif

#ifndef VM_SCHEDULE_TIMEOUT
#define VM_SCHEDULE_TIMEOUT 2
#endif

/* Maximum recursion depth (number of vm_expr_t* slots) per thread.
   Each slot is one pointer, so the per-thread cost is small. */
#ifndef VM_CONTEXT_STACK_SIZE
#if CONTIKI_TARGET_ZOUL
#define VM_CONTEXT_STACK_SIZE 30
#elif CONTIKI_TARGET_NATIVE
#define VM_CONTEXT_STACK_SIZE 64
#else
#define VM_CONTEXT_STACK_SIZE 48
#endif
#endif

#ifndef VM_OBJECT_STACK_SIZE
#if CONTIKI_TARGET_ZOUL
#define VM_OBJECT_STACK_SIZE 10
#else
/* Each vm_expr_t carries an argv[] of this size. Compiled apps from
   pyvelox can lower a literal like `range(10)` to a (list 0 1 ... 9)
   form whose width exceeds 10, tripping a "too many arguments" stack
   overflow at the call site. 16 matches the POSIX port. Zoul keeps
   the tighter ceiling because of its 32 kB RAM budget. */
#define VM_OBJECT_STACK_SIZE 16
#endif
#endif

#ifndef VM_ERROR_MESSAGES
#define VM_ERROR_MESSAGES 1
#endif

#ifndef VM_LIST_PRINT_LIMIT
#define VM_LIST_PRINT_LIMIT 10
#endif

#ifndef VM_BUFFER_PRINT_LIMIT
#define VM_BUFFER_PRINT_LIMIT 64
#endif

/* Build an executable of the VM and an app stored
   as bytecode in a header file. */
#ifndef VM_BUNDLE
#define VM_BUNDLE 0
#endif

#endif /* !VM_CONFIG_H */
