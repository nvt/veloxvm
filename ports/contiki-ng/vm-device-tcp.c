/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB.
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

/*
 * I/O ops for VM ports backed by a Contiki-NG tcp-socket. Mirrors
 * vm-device-uip.c (the UDP equivalent) but reads from / writes to a
 * struct native_tcp_socket and tracks connection state so reads
 * before CONNECTED block, and reads after CLOSED return EOF.
 */

#include "contiki.h"
#include "net/ipv6/tcp-socket.h"

#include "vm.h"
#include "vm-log.h"
#include "vm-native.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
static int
vm_tcp_read(vm_port_t *port, char *buf, size_t size)
{
  struct native_tcp_socket *sock;
  size_t avail;
  size_t n;

  sock = port->opaque_desc;
  if(sock == NULL || size == 0) {
    return 0;
  }

  /* Drain whatever is buffered first, even if the connection closed
     after the data arrived -- callers expect to see the tail of the
     stream before EOF. */
  avail = sock->rx_len - sock->rx_pos;
  if(avail > 0) {
    n = avail < size ? avail : size;
    memcpy(buf, sock->rx_buf + sock->rx_pos, n);
    sock->rx_pos += n;
    if(sock->rx_pos >= sock->rx_len) {
      sock->rx_pos = 0;
      sock->rx_len = 0;
    }
    return (int)n;
  }

  if(sock->state == NATIVE_TCP_CLOSED) {
    /* Connection gone and nothing left buffered. Returning 0 maps to
       EOF for the read primitives in expr-io.c. */
    return 0;
  }

  /* Connection still alive (PENDING or CONNECTED) but no bytes yet.
     Park the calling thread; the data / event callback will wake it
     when something changes. */
  if(port->thread != NULL) {
    vm_native_sleep(port->thread, VM_POLL_TIME);
    if(port->thread->expr != NULL) {
      VM_SET_FLAG(port->thread->expr->flags, VM_EXPR_RESTART);
    }
  }
  return 0;
}
/*---------------------------------------------------------------------------*/
static int
vm_tcp_write(vm_port_t *port, const char *buf, size_t size)
{
  struct native_tcp_socket *sock;

  sock = port->opaque_desc;
  if(sock == NULL || sock->state == NATIVE_TCP_CLOSED) {
    return -1;
  }

  /* Writes before CONNECTED are queued by tcp-socket.c and flushed
     once the handshake completes, so we do not need to spin here. */
  return tcp_socket_send(&sock->socket, (const uint8_t *)buf, (int)size);
}
/*---------------------------------------------------------------------------*/
static void
vm_tcp_close(vm_port_t *port)
{
  struct native_tcp_socket *sock;

  sock = port->opaque_desc;
  if(sock == NULL) {
    return;
  }
  sock->port = NULL;
  tcp_socket_close(&sock->socket);
  tcp_socket_unregister(&sock->socket);
  vm_native_release_tcp_socket(sock);
  port->opaque_desc = NULL;
}
/*---------------------------------------------------------------------------*/
const vm_port_io_t device_tcp = {
  .open = NULL,
  .read = vm_tcp_read,
  .read_object = NULL,
  .write = vm_tcp_write,
  .close = vm_tcp_close
};
/*---------------------------------------------------------------------------*/
