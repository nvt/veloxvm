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
 */

#ifndef VM_PORT_H
#define VM_PORT_H

#include "contiki.h"
#include "cfs/cfs.h"
#include "lib/random.h"
#include "net/ipv6/udp-socket.h"
#include "net/ipv6/uip.h"

#include "lib/heapmem.h"
#include "vm-file.h"
#include "vm-config.h"

#include <stdbool.h>
#include <stdint.h>

#define VM_MALLOC vm_native_alloc
#define VM_FREE vm_native_free
#define VM_REALLOC vm_native_realloc
#define VM_STRDUP vm_native_strdup

typedef int vm_loader_handle_t;
typedef int vm_loader_offset_t;
#define VM_LOADER_OPEN(name) vm_file_open((name), 0)
#define VM_LOADER_READ(handle, buf, size) vm_file_read((handle), (buf), (size))
#define VM_LOADER_SEEK_RELATIVE(handle, offset) \
  vm_file_seek((handle), (offset), VM_FILE_SEEK_CUR)
#define VM_LOADER_SEEK_ABSOLUTE(handle, offset) \
  vm_file_seek((handle), (offset), VM_FILE_SEEK_SET)
#define VM_LOADER_CLOSE(handle) vm_file_close(handle)

#define VM_MEMPOOL_CONF_ALLOC vm_native_alloc
#define VM_MEMPOOL_CONF_FREE vm_native_free

#define VM_RANDOM_FUNCTION random_rand

typedef rtimer_clock_t vm_native_time_t;
#define VM_NATIVE_TIME() RTIMER_NOW()
/* The native clocks resolution in Hertz. */
#define VM_NATIVE_TIME_RESOLUTION() RTIMER_SECOND

/* Forward declaration so the back-pointer below does not require
   including vm.h. */
struct vm_port;

struct native_socket {
  struct udp_socket socket;
  uint16_t proto;
  uint16_t lport;
  uint16_t rport;

  /* Back-pointer set by vm_native_open_client so the udp_input
     callback can find the owning port (and thus the parked reader
     thread) without searching the socket list. */
  struct vm_port *port;

  /* Source of the most recent inbound datagram. peer-name returns
     this rather than the connected ripaddr so server-style flows can
     tell who sent what. */
  uip_ipaddr_t last_src_addr;
  uint16_t last_src_port;
  bool last_src_valid;

  /* Single in-flight inbound datagram. udp_input copies the payload
     here; vm_uip_read drains rx_pos..rx_len. New datagrams arriving
     while rx_len > rx_pos are dropped (logged) -- the caller must
     read promptly. A ring would help but VM_SOCKET_RX_BUFSIZE is
     already pre-allocated per slot so a fixed single-slot buffer
     keeps memory linear in VM_MAX_SOCKETS. */
  uint8_t rx_buf[VM_SOCKET_RX_BUFSIZE];
  uint16_t rx_pos;
  uint16_t rx_len;
};

extern void *vm_native_alloc(size_t);
extern bool vm_native_free(void *);
extern void *vm_native_realloc(void *, size_t);
extern char *vm_native_strdup(const char *);
extern void vm_native_release_socket(struct native_socket *);

PROCESS_NAME(vm_perfmon_process);

#endif /* !VM_PORT_H */
