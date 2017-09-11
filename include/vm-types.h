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
 */

#ifndef VM_TYPES_H
#define VM_TYPES_H

/* Instruction pointer. */
typedef uint8_t *vm_ip_t;

/* Time representation. */
typedef struct vm_time {
  uint32_t sec;
  uint16_t msec;
} vm_time_t;

/* Scheduler commands. */
typedef enum vm_sched_command {
  VM_SCHED_RETURN         = 0,
  VM_SCHED_CONTINUE       = 1,
  VM_SCHED_PROCESS_RESULT = 2,
  VM_SCHED_JUMP_NEXT      = 3
} vm_sched_command_t;

/* Thread scheduling result. */
typedef enum vm_result {
  VM_RESULT_FINISHED      = 0,
  VM_RESULT_SLEEPING      = 1,
  VM_RESULT_RUNNING       = 2
} vm_result_t;

/* Thread execution status. */
typedef enum vm_thread_status {
  VM_THREAD_RUNNABLE      = 0,
  VM_THREAD_WAITING       = 1,
  VM_THREAD_ERROR         = 2,
  VM_THREAD_EXITING       = 3,
  VM_THREAD_FINISHED      = 4
} vm_thread_status_t;

/* Various statistics for individual threads. */
typedef struct vm_thread_stats {
  uint32_t schedulings;
  uint32_t function_calls;
  uint32_t allocated_total;
} vm_thread_stats_t;

/* Identifier for VM threads. */
typedef uint8_t vm_thread_id_t;

/* Socket types. */
typedef enum vm_socket_type {
  VM_SOCKET_STREAM        = 0,
  VM_SOCKET_DATAGRAM      = 1
} vm_socket_type_t;


#endif /* !VM_TYPES_H */
