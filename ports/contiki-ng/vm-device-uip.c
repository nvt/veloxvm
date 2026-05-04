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
 * Author: Nicolas Tsiftes <nvt@acm.org>,
 */

#include "contiki.h"
#include "net/ipv6/uip.h"
#include "net/ipv6/udp-socket.h"

#include "vm.h"
#include "vm-log.h"
#include "vm-native.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
static int
vm_uip_read(vm_port_t *port, char *buf, size_t size)
{
  struct native_socket *sock;
  size_t avail;
  size_t n;

  sock = port->opaque_desc;
  if(sock == NULL || size == 0) {
    return 0;
  }

  avail = sock->rx_len - sock->rx_pos;
  if(avail == 0) {
    /* No bytes buffered yet. Park the calling thread on a short timer
       so the eval loop retries; udp_input wakes the thread early when
       a datagram actually arrives. The expr-restart flag is what
       rewinds us back to this read on the next pass. */
    if(port->thread != NULL) {
      vm_native_sleep(port->thread, VM_POLL_TIME);
      if(port->thread->expr != NULL) {
        VM_SET_FLAG(port->thread->expr->flags, VM_EXPR_RESTART);
      }
    }
    return 0;
  }

  n = avail < size ? avail : size;
  memcpy(buf, sock->rx_buf + sock->rx_pos, n);
  sock->rx_pos += n;
  if(sock->rx_pos >= sock->rx_len) {
    sock->rx_pos = 0;
    sock->rx_len = 0;
  }
  return (int)n;
}
/*---------------------------------------------------------------------------*/
static int
vm_uip_write(vm_port_t *port, const char *buf, size_t size)
{
  struct native_socket *sock;

  sock = port->opaque_desc;
  if(sock == NULL) {
    return -1;
  }
  return udp_socket_send(&sock->socket, buf, size);
}
/*---------------------------------------------------------------------------*/
static void
vm_uip_close(vm_port_t *port)
{
  struct native_socket *sock;

  sock = port->opaque_desc;
  if(sock == NULL) {
    return;
  }
  /* Drop the back-pointer first: udp_input might fire one more time
     between udp_socket_close and the memb release on some stacks.
     With sock->port cleared it becomes a logged drop, not a UAF. */
  sock->port = NULL;
  udp_socket_close(&sock->socket);
  /* Reclaim the MEMB slot here so both manual closes and GC-driven
     port finalisations free it. Skipping this leaks the slot until
     reboot and eventually starves allocate_socket(). */
  vm_native_release_socket(sock);
  port->opaque_desc = NULL;
}
/*---------------------------------------------------------------------------*/
const vm_port_io_t device_uip = {
  .open = NULL,
  .read = vm_uip_read,
  .read_object = NULL,
  .write = vm_uip_write,
  .close = vm_uip_close
};
/*---------------------------------------------------------------------------*/
