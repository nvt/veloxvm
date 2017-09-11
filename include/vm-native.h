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

#ifndef VM_NATIVE_H
#define VM_NATIVE_H

#include "vm.h"

int vm_native_init(void);
const char *vm_native_get_os_version(void);
vm_program_t *vm_native_read_program(const char *);
void vm_native_poll(void);
void vm_native_sleep(vm_thread_t *, vm_integer_t);
int vm_native_time(vm_time_t *);
void vm_native_close_port(vm_port_t *);
vm_port_t *vm_native_default_port(vm_thread_t *, int);
vm_port_t *vm_native_open_client(vm_thread_t *, vm_socket_type_t,
                                 vm_vector_t *, vm_integer_t, vm_integer_t);
vm_port_t *vm_native_open_server(vm_thread_t *, vm_vector_t *, vm_integer_t);
int vm_native_get_peer_name(vm_thread_t *, vm_port_t *, vm_obj_t *);
void vm_native_accept_client(vm_thread_t *, vm_port_t *, vm_obj_t *);
void vm_native_incoming_clientp(vm_thread_t *, vm_port_t *, vm_obj_t *);
int vm_native_resolve(vm_thread_t *, const char *hostname);
vm_port_t *vm_native_open_file(vm_thread_t *, const char *, int);
int vm_native_read(vm_port_t *, vm_obj_t *);
int vm_native_read_char(vm_port_t *, vm_character_t *);
int vm_native_peek_char(vm_port_t *, vm_character_t *);
int vm_native_write(vm_port_t *, const char *, ...);
int vm_native_write_buffer(vm_port_t *, const char *, size_t);
vm_boolean_t vm_native_char_readyp(vm_port_t *);
unsigned vm_native_calculate_power(const vm_program_t *);
void vm_native_accounting_start(vm_thread_t *);
void vm_native_accounting_stop(vm_thread_t *);

/* Introduce a shorter name for the often used generic write function. */
#define vm_write vm_native_write

#if VM_SERVER
int vm_server_init(void);
void vm_server_shutdown(void);
void vm_server_close(vm_port_t *);
void vm_server_update(void);
void vm_server_incoming(int);
#endif

#endif /* !VM_NATIVE_H */
