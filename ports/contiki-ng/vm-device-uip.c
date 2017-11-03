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

/*---------------------------------------------------------------------------*/
static int
vm_uip_read(vm_port_t *port, char *buf, size_t size)
{
  return 0;
}
/*---------------------------------------------------------------------------*/
static int
vm_uip_write(vm_port_t *port, const char *buf, size_t size)
{
  struct native_socket *sock;

  sock = port->opaque_desc;
  return udp_socket_send(&sock->socket, buf, size);
}
/*---------------------------------------------------------------------------*/
static void
vm_uip_close(vm_port_t *port)
{
  struct native_socket *sock;

  sock = port->opaque_desc;
  udp_socket_close(&sock->socket);
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
