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

#include <stdarg.h>
#include <stdio.h>

#include "contiki.h"
#include "cfs/cfs.h"
#include "cfs/cfs-coffee.h"
#include "lib/heapmem.h"
#include "lib/list.h"
#include "lib/memb.h"
#include "lib/sensors.h"
#include "net/ipv6/uip.h"
#include "net/ipv6/uiplib.h"
#include "net/ipv6/udp-socket.h"
#include "net/ipv6/uip-nameserver.h"
#include "net/ipv6/uip-ds6.h"
#include "resolv.h"
#include "sys/energest.h"

#include "sys/log.h"
#define LOG_MODULE "VM"
#define LOG_LEVEL LOG_LEVEL_INFO

#include "vm-log.h"
#include "vm-native.h"

HEAPMEM_ZONE_DEFINE(vm_heap_zone, VM_ZONE_SIZE);

void *
vm_native_alloc(size_t size)
{
  return heapmem_zone_alloc(&vm_heap_zone, size);
}

bool
vm_native_free(void *ptr)
{
  return heapmem_zone_free(&vm_heap_zone, ptr);
}

void *
vm_native_realloc(void *ptr, size_t size)
{
  return heapmem_zone_realloc(&vm_heap_zone, ptr, size);
}

PROCESS_NAME(vm_process);

extern const vm_port_io_t device_cfs;
extern const vm_port_io_t device_leds;
extern const vm_port_io_t device_sensors;
extern const vm_port_io_t device_serial;
extern const vm_port_io_t device_tcp;
extern const vm_port_io_t device_uip;

static struct ctimer timers[VM_THREAD_AMOUNT];

MEMB(socket_memb, struct native_socket, VM_MAX_SOCKETS);
LIST(socket_list);

MEMB(tcp_socket_memb, struct native_tcp_socket, VM_MAX_TCP_SOCKETS);
LIST(tcp_socket_list);

static vm_port_t serial_port = {.fd = 0, .io = &device_serial,
                                .flags = VM_PORT_FLAG_OPEN    |
                                         VM_PORT_FLAG_CONSOLE |
                                         VM_PORT_FLAG_INPUT   |
                                         VM_PORT_FLAG_OUTPUT};

static void
thread_timer_expired(void *arg)
{
  vm_thread_t *thread;
  vm_id_index_t index;

  thread = arg;
  index = vm_thread_get_index(thread);
  if(index == VM_ID_INDEX_INVALID) {
    VM_DEBUG(VM_DEBUG_HIGH, "Invalid thread ID in %s", __func__);
    return;
  }

  thread->status = VM_THREAD_RUNNABLE;
  process_poll(&vm_process);

  VM_DEBUG(VM_DEBUG_HIGH, "The timer expired for thread ID %lu",
           (unsigned long)thread->id);
}

static struct native_socket *
allocate_socket(void)
{
  struct native_socket *sock;

  sock = memb_alloc(&socket_memb);
  if(sock != NULL) {
    /* memb_alloc does not zero the slot; clear the bookkeeping fields
       so the rx queue starts empty and the back-pointer is well
       defined before udp_socket_register exposes us to callbacks. */
    memset(sock, 0, sizeof(*sock));
    list_push(socket_list, sock);
  }
  return sock;
}

/* Release a socket slot. Non-static because vm-device-uip.c needs to
   call it from its close handler -- that path runs both for explicit
   closes and for the GC-sweep finaliser, so this is the single point
   where the memb slot is reclaimed. */
void
vm_native_release_socket(struct native_socket *sock)
{
  if(sock == NULL) {
    return;
  }
  list_remove(socket_list, sock);
  memb_free(&socket_memb, sock);
}

static struct native_tcp_socket *
allocate_tcp_socket(void)
{
  struct native_tcp_socket *sock;

  sock = memb_alloc(&tcp_socket_memb);
  if(sock != NULL) {
    memset(sock, 0, sizeof(*sock));
    list_push(tcp_socket_list, sock);
  }
  return sock;
}

void
vm_native_release_tcp_socket(struct native_tcp_socket *sock)
{
  if(sock == NULL) {
    return;
  }
  list_remove(tcp_socket_list, sock);
  memb_free(&tcp_socket_memb, sock);
}

static int
vector_to_ip(vm_vector_t *vector, uip_ipaddr_t *addr)
{
  int i;

  if(vector->length == 4 || vector->length == 16) {
    for(i = 0; i < vector->length; i++) {
      if(vector->elements[i].type != VM_TYPE_INTEGER) {
        return 0;
      }
      addr->u8[i] = vector->elements[i].value.integer;
    }

    return 1;
  } else if(vector->length == 8) {
    for(i = 0; i < vector->length; i++) {
      if(vector->elements[i].type != VM_TYPE_INTEGER) {
        return 0;
      }
      addr->u16[i] = uip_htons(vector->elements[i].value.integer);
    }

    return 1;
  } else {
    return 0;
  }
}

static void
ip_to_vector(uip_ipaddr_t *addr, vm_vector_t *vector)
{
  int i;

  for(i = 0; i < sizeof(uip_ipaddr_t); i++) {
    vector->elements[i].type = VM_TYPE_INTEGER;
    vector->elements[i].value.integer = addr->u8[i];
  }
}

static void
time_diff(vm_time_t *end, vm_time_t *start, vm_time_t *diff)
{
  int16_t msec;

  diff->sec = end->sec - start->sec;
  msec = (int16_t)end->msec - (int16_t)start->msec;
  if(msec < 0) {
    msec += 1000;
    diff->sec--;
  }
  diff->msec = msec;
}

static void
attribute_bandwidth(vm_thread_t *thread, uint16_t bytes)
{
  vm_time_t current_time;
  vm_time_t diff;
  uint64_t diff_msec;
  uint32_t bps;

  vm_native_time(&current_time);
  time_diff(&current_time, &thread->program->perf_attr.last_comm, &diff);
  VM_TIME_COPY(thread->program->perf_attr.last_comm, current_time);

  diff_msec = (uint64_t)diff.sec * 1000 + diff.msec;
  /* Two events inside the same millisecond reduce diff_msec to 0.
     Clamp so the rate filter still gets a sample. */
  if(diff_msec == 0) {
    diff_msec = 1;
  }
  bps = (uint64_t)1000 * 8 * bytes / diff_msec;

#if 0
  printf("diff %lu.%03u - %lu.%03u = %lu.%03u\n",
    (unsigned long)current_time.sec, (unsigned)current_time.msec,
    (unsigned long)thread->program->perf_attr.last_comm.sec, (unsigned)thread->program->perf_attr.last_comm.msec,
    (unsigned long)diff.sec, (unsigned)diff.msec);
  printf("%u bytes, %u bps\n", (unsigned)bytes, (unsigned)bps);
#endif

  vm_filter_put(&thread->program->perf_attr.bandwidth, bps);
}

static void
attribute_communication(vm_thread_t *thread, uint16_t proto, uint16_t port)
{
  /* No Powertrace in Contiki-NG. */
}

/* Wake a thread parked by vm_uip_read. Cancels the polling timer and
   nudges the VM process so the eval loop picks the thread up on its
   next pass. Safe to call from the tcpip-process context where
   udp_input runs because Contiki-NG schedules processes
   cooperatively -- there is no preemption between this and the VM
   eval loop. */
static void
wake_thread(vm_thread_t *thread)
{
  vm_id_index_t idx;

  if(thread == NULL) {
    return;
  }
  idx = vm_thread_get_index(thread);
  if(idx != VM_ID_INDEX_INVALID) {
    ctimer_stop(&timers[idx]);
  }
  if(thread->status == VM_THREAD_WAITING) {
    thread->status = VM_THREAD_RUNNABLE;
    process_poll(&vm_process);
  }
}

static void
udp_input(struct udp_socket *sock, void *ptr,
          const uip_ipaddr_t *source_addr, uint16_t source_port,
          const uip_ipaddr_t *dest_addr, uint16_t dest_port,
          const uint8_t *data, uint16_t datalen)
{
  struct native_socket *nsock;
  vm_port_t *port;
  vm_thread_t *thread;
  uint16_t copy;

  nsock = ptr;
  if(nsock == NULL) {
    return;
  }
  port = nsock->port;
  thread = port != NULL ? port->thread : NULL;

  if(thread != NULL) {
    attribute_communication(thread, UIP_PROTO_UDP,
                            VM_MIN(uip_ntohs(source_port),
                                   uip_ntohs(dest_port)));
    VM_DEBUG(VM_DEBUG_MEDIUM,
             "Program %s received a UDP packet of %u bytes",
             thread->program->name, (unsigned)datalen);
  }

  if(datalen == 0) {
    return;
  }

  /* Drop the new datagram if the previous one has not been drained.
     UDP semantics already permit loss; logging makes the situation
     visible without aborting the connection. */
  if(nsock->rx_pos < nsock->rx_len) {
    VM_DEBUG(VM_DEBUG_LOW,
             "UDP rx slot full, dropping %u bytes", (unsigned)datalen);
    return;
  }

  copy = datalen > VM_SOCKET_RX_BUFSIZE ? VM_SOCKET_RX_BUFSIZE : datalen;
  if(copy < datalen) {
    VM_DEBUG(VM_DEBUG_LOW,
             "UDP rx datagram %u bytes truncated to %u",
             (unsigned)datalen, (unsigned)copy);
  }
  memcpy(nsock->rx_buf, data, copy);
  nsock->rx_pos = 0;
  nsock->rx_len = copy;

  uip_ipaddr_copy(&nsock->last_src_addr, source_addr);
  nsock->last_src_port = source_port;
  nsock->last_src_valid = true;

  wake_thread(thread);
}

/* Drain incoming TCP bytes into the per-socket rx slot. tcp-socket.c
   contracts:
     - the callback should return how many bytes to LEAVE in its
       input buffer for the next call.
     - returning 0 consumes everything; the library is then free to
       reuse the buffer immediately.
   We copy whatever fits in our rx slot, ask the library to drop the
   consumed prefix, and tell it to keep the rest for the next call.
   That way the TCP receive window naturally throttles when the VM
   reader is slow. */
static int
tcp_data_callback(struct tcp_socket *socket, void *ptr,
                  const uint8_t *input_data, int input_len)
{
  struct native_tcp_socket *nsock;
  vm_port_t *port;
  vm_thread_t *thread;
  uint16_t free_bytes;
  uint16_t copy;

  nsock = ptr;
  if(nsock == NULL || input_len <= 0) {
    return 0;
  }
  port = nsock->port;
  thread = port != NULL ? port->thread : NULL;

  if(nsock->rx_pos < nsock->rx_len) {
    /* Reader has not drained yet -- ask the library to keep
       everything until next time so we do not lose bytes. */
    return input_len;
  }

  free_bytes = sizeof(nsock->rx_buf);
  copy = (uint16_t)input_len > free_bytes ? free_bytes : (uint16_t)input_len;
  memcpy(nsock->rx_buf, input_data, copy);
  nsock->rx_pos = 0;
  nsock->rx_len = copy;

  if(thread != NULL) {
    attribute_communication(thread, UIP_PROTO_TCP,
                            VM_MIN(nsock->lport, nsock->rport));
  }
  wake_thread(thread);

  /* Anything we did not absorb stays buffered; the library will hand
     it back the next time the reader calls us. */
  return input_len - copy;
}

static void
tcp_event_callback(struct tcp_socket *socket, void *ptr,
                   tcp_socket_event_t event)
{
  struct native_tcp_socket *nsock;
  vm_port_t *port;
  vm_thread_t *thread;

  nsock = ptr;
  if(nsock == NULL) {
    return;
  }
  port = nsock->port;
  thread = port != NULL ? port->thread : NULL;

  switch(event) {
  case TCP_SOCKET_CONNECTED:
    nsock->state = NATIVE_TCP_CONNECTED;
    /* Capture the peer's address from the underlying uIP connection
       so peer-name returns something meaningful for sockets that
       transitioned from listening to connected. tcp-socket.c does
       not expose a getter, so we read directly from socket->c. */
    if(socket->c != NULL) {
      uip_ipaddr_copy(&nsock->peer_addr, &socket->c->ripaddr);
      nsock->peer_port = uip_ntohs(socket->c->rport);
    }
    VM_DEBUG(VM_DEBUG_LOW, "TCP connected");
    break;
  case TCP_SOCKET_CLOSED:
  case TCP_SOCKET_TIMEDOUT:
  case TCP_SOCKET_ABORTED:
    nsock->state = NATIVE_TCP_CLOSED;
    VM_DEBUG(VM_DEBUG_LOW, "TCP closed/aborted (event %d)", (int)event);
    break;
  case TCP_SOCKET_DATA_SENT:
    /* Output buffer drained; nothing to do here for now. */
    break;
  default:
    break;
  }

  /* Any state change can unblock a reader: connect completing means
     writes can flow, close means reads should return EOF. */
  wake_thread(thread);
}

static void
attribute_energy(vm_thread_t *thread, int done)
{
  static unsigned long e_cpu, e_lpm;
  unsigned long diff_cpu, diff_lpm, cpu_time, total_time;

  if(!done) {
    e_cpu      = energest_type_time(ENERGEST_TYPE_CPU);
    e_lpm      = energest_type_time(ENERGEST_TYPE_LPM);
  } else {
    diff_cpu      = energest_type_time(ENERGEST_TYPE_CPU) - e_cpu;
    diff_lpm      = energest_type_time(ENERGEST_TYPE_LPM) - e_lpm;

    thread->program->perf_attr.cpu_time += diff_cpu;

    total_time = diff_cpu + diff_lpm + e_cpu + e_lpm;
    cpu_time = thread->program->perf_attr.cpu_time;

    if(total_time > 0) {
      /* Multiply by hundred because the policy is for percentages. */
      vm_policy_check_cpu(thread->program, 100 * cpu_time / total_time);
    }

    vm_policy_check_power(thread);
  }
}

static int
register_devices(void)
{
  static const struct {
    const char *name;
    const vm_port_io_t *io;
    uint8_t flags;
  } devs[] = {
    { "cfs",    &device_cfs,    VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT },
    { "leds",   &device_leds,   VM_PORT_FLAG_OUTPUT },
    { "serial", &device_serial, VM_PORT_FLAG_INPUT },
  };
  size_t i;
  int ok = 1;

  for(i = 0; i < VM_ARRAY_SIZE(devs); i++) {
    if(vm_device_register(devs[i].name, devs[i].io, devs[i].flags) == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Failed to register %s", devs[i].name);
      ok = 0;
    }
  }
  return ok;
}

int
vm_native_init(void)
{
  uip_ipaddr_t dns_addr;

  if(!register_devices()) {
    /* Without the core devices the VM cannot run any non-trivial
       program; refuse to come up so the failure is visible at boot
       instead of surfacing later as opaque IO errors. */
    return 0;
  }

  /* Default to a nameserver at the border router's host. Override by
     defining VM_DNS_SERVER_ADDR{0..7} in vm-config.h to inject a fixed
     resolver, or rely on RA-supplied servers if the network advertises
     them. */
#ifdef VM_DNS_SERVER_ADDR0
  uip_ip6addr(&dns_addr,
              VM_DNS_SERVER_ADDR0, VM_DNS_SERVER_ADDR1,
              VM_DNS_SERVER_ADDR2, VM_DNS_SERVER_ADDR3,
              VM_DNS_SERVER_ADDR4, VM_DNS_SERVER_ADDR5,
              VM_DNS_SERVER_ADDR6, VM_DNS_SERVER_ADDR7);
#else
  uip_ip6addr(&dns_addr, UIP_DS6_DEFAULT_PREFIX, 0, 0, 0, 0, 0, 0, 1);
#endif
  uip_nameserver_update(&dns_addr, 1000000);

#if VM_SERVER
  if(vm_control_init()) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to initialize the VM control interface");
    return 0;
  }
#endif

  return 1;
}

const char *
vm_native_get_os_version(void)
{
#ifdef CONTIKI_VERSION_STRING
  return CONTIKI_VERSION_STRING;
#else
  return "Contiki-NG";
#endif
}

void
vm_native_poll(void)
{
}

void
vm_native_sleep(vm_thread_t *thread, vm_integer_t ms)
{
  vm_id_index_t index;
  uint64_t ticks;
  clock_time_t timer_ticks;

  index = vm_thread_get_index(thread);
  if(index == VM_ID_INDEX_INVALID) {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
    return;
  }

  /* Compute the timer expiration in clock ticks via uint64_t. The naive
     CLOCK_SECOND * ms overflows int once CLOCK_SECOND >= 128 and ms is
     larger than ~16M. Clamp negative values to zero and saturate at the
     widest tick value the underlying ctimer can hold. */
  if(ms < 0) {
    ms = 0;
  }
  ticks = ((uint64_t)CLOCK_SECOND * (uint64_t)ms) / 1000;
  if(ticks > (clock_time_t)-1) {
    timer_ticks = (clock_time_t)-1;
  } else {
    timer_ticks = (clock_time_t)ticks;
  }

  ctimer_set(&timers[index], timer_ticks, thread_timer_expired, thread);
  thread->status = VM_THREAD_WAITING;
}

int
vm_native_time(vm_time_t *time)
{
  time->sec = clock_seconds();
  time->msec = ((RTIMER_NOW() % RTIMER_SECOND) * 1000UL) / RTIMER_SECOND;
  return 1;
}

void
vm_native_close_port(vm_port_t *port)
{
  /* Per-IO close also reclaims the underlying native_socket for socket
     ports (see vm-device-uip.c). The same path runs from the GC-sweep
     finaliser in core/vm-memory.c, so leak coverage is uniform. */
  if(port->io && port->io->close) {
    port->io->close(port);
  }

  port->flags &= ~VM_PORT_FLAG_OPEN;
}

vm_port_t *
vm_native_default_port(vm_thread_t *thread, int direction)
{
  return &serial_port;
}

static vm_port_t *
open_udp_client(vm_thread_t *thread, const uip_ipaddr_t *addr,
                vm_integer_t dest_port, vm_integer_t source_port)
{
  struct native_socket *sock;
  vm_port_t *port;

  sock = allocate_socket();
  if(sock == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to allocate a socket");
    return NULL;
  }

  /* Allocate the port up front so we can bind it to the socket before
     registering with the network stack. udp_input may fire as soon as
     the socket is bound, and it dereferences sock->port to find the
     reader thread; an unset port pointer would crash. */
  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    vm_native_release_socket(sock);
    return NULL;
  }
  vm_port_register(port);

  port->thread = thread;
  port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;
  port->io = &device_uip;
  port->opaque_desc = sock;

  sock->port = port;

  if(udp_socket_register(&sock->socket, sock, udp_input) < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to create a UDP socket");
    sock->port = NULL;
    port->opaque_desc = NULL;
    port->flags = 0;
    vm_native_release_socket(sock);
    return NULL;
  }

  sock->proto = UIP_PROTO_UDP;
  if(source_port != 0) {
    sock->lport = source_port;
  } else {
    sock->lport = (32768 + random_rand()) & 0xffff;
  }
  sock->rport = dest_port;

  udp_socket_bind(&sock->socket, sock->lport);

  if(udp_socket_connect(&sock->socket, addr, dest_port) < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to connect a UDP socket");
    udp_socket_close(&sock->socket);
    sock->port = NULL;
    port->opaque_desc = NULL;
    port->flags = 0;
    vm_native_release_socket(sock);
    return NULL;
  }

  LOG_INFO("Created a UDP connection to ");
  LOG_INFO_6ADDR(addr);
  LOG_INFO("\n");

  return port;
}

static vm_port_t *
open_tcp_client(vm_thread_t *thread, const uip_ipaddr_t *addr,
                vm_integer_t dest_port)
{
  struct native_tcp_socket *sock;
  vm_port_t *port;

  sock = allocate_tcp_socket();
  if(sock == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to allocate a TCP socket");
    return NULL;
  }

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    vm_native_release_tcp_socket(sock);
    return NULL;
  }
  vm_port_register(port);

  port->thread = thread;
  port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;
  port->io = &device_tcp;
  port->opaque_desc = sock;

  sock->port = port;
  sock->state = NATIVE_TCP_PENDING;
  sock->rport = dest_port;
  uip_ipaddr_copy(&sock->peer_addr, addr);
  sock->peer_port = dest_port;

  if(tcp_socket_register(&sock->socket, sock,
                         sock->inbuf, sizeof(sock->inbuf),
                         sock->outbuf, sizeof(sock->outbuf),
                         tcp_data_callback, tcp_event_callback) < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to register a TCP socket");
    sock->port = NULL;
    port->opaque_desc = NULL;
    port->flags = 0;
    vm_native_release_tcp_socket(sock);
    return NULL;
  }

  if(tcp_socket_connect(&sock->socket, addr, dest_port) < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to start TCP connect");
    tcp_socket_unregister(&sock->socket);
    sock->port = NULL;
    port->opaque_desc = NULL;
    port->flags = 0;
    vm_native_release_tcp_socket(sock);
    return NULL;
  }

  LOG_INFO("Started TCP connect to ");
  LOG_INFO_6ADDR(addr);
  LOG_INFO(":%u\n", (unsigned)dest_port);

  /* Returns immediately; reads/writes block until the event callback
     transitions sock->state to NATIVE_TCP_CONNECTED. */
  return port;
}

vm_port_t *
vm_native_open_client(vm_thread_t *thread, vm_socket_type_t socket_type,
                      vm_vector_t *address, vm_integer_t dest_port,
                      vm_integer_t source_port)
{
  uip_ipaddr_t addr;

  if(vector_to_ip(address, &addr) == 0) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to convert an IP address");
    return NULL;
  }

  switch(socket_type) {
  case VM_SOCKET_DATAGRAM:
    return open_udp_client(thread, &addr, dest_port, source_port);
  case VM_SOCKET_STREAM:
    return open_tcp_client(thread, &addr, dest_port);
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_string(thread, "unknown socket type");
    return NULL;
  }
}

/* Set up a TCP listener on the given native_tcp_socket. Used both
   when first opening the server and when replenishing the listener
   slot after accept-client hands the connected one off to a new
   client port. */
static int
start_tcp_listen(struct native_tcp_socket *sock, vm_integer_t listen_port)
{
  if(tcp_socket_register(&sock->socket, sock,
                         sock->inbuf, sizeof(sock->inbuf),
                         sock->outbuf, sizeof(sock->outbuf),
                         tcp_data_callback, tcp_event_callback) < 0) {
    return 0;
  }
  if(tcp_socket_listen(&sock->socket, listen_port) < 0) {
    tcp_socket_unregister(&sock->socket);
    return 0;
  }
  sock->state = NATIVE_TCP_PENDING;
  sock->lport = listen_port;
  return 1;
}

vm_port_t *
vm_native_open_server(vm_thread_t *thread,
                   vm_vector_t *address,
                   vm_integer_t listen_port)
{
  /* Bind address is ignored: tcp-socket.c only takes a port and
     listens on every local IPv6 address. The argument stays in the
     API for symmetry with the POSIX port and for future expansion. */
  struct native_tcp_socket *sock;
  vm_port_t *port;

  (void)address;

  sock = allocate_tcp_socket();
  if(sock == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to allocate a TCP socket");
    return NULL;
  }

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    vm_native_release_tcp_socket(sock);
    return NULL;
  }
  vm_port_register(port);

  port->thread = thread;
  port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;
  port->io = &device_tcp;
  port->opaque_desc = sock;
  sock->port = port;

  if(!start_tcp_listen(sock, listen_port)) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to start TCP listen on port %ld",
             (long)listen_port);
    sock->port = NULL;
    port->opaque_desc = NULL;
    port->flags = 0;
    vm_native_release_tcp_socket(sock);
    return NULL;
  }

  LOG_INFO("Listening on TCP port %u\n", (unsigned)listen_port);
  return port;
}


static int
peer_addr_to_vector(vm_thread_t *thread, const uip_ipaddr_t *peeraddr,
                    vm_obj_t *obj)
{
  vm_vector_t *vector;
  vm_obj_t item;
  unsigned i;

  vector = vm_vector_create(obj, sizeof(peeraddr->u8), VM_VECTOR_FLAG_REGULAR);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return 0;
  }

  item.type = VM_TYPE_INTEGER;
  for(i = 0; i < sizeof(peeraddr->u8); i++) {
    item.value.integer = peeraddr->u8[i];
    vm_vector_set(obj, i, &item);
  }

  return 1;
}

int
vm_native_get_peer_name(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  const uip_ipaddr_t *peeraddr;

  if(port->io == &device_tcp) {
    struct native_tcp_socket *tsock = port->opaque_desc;
    if(tsock == NULL) {
      return 0;
    }
    peeraddr = &tsock->peer_addr;
  } else {
    struct native_socket *sock = port->opaque_desc;
    if(sock == NULL) {
      return 0;
    }
    /* Prefer the source of the last datagram we received (so server
       flows can tell who sent what). Fall back to the connected
       ripaddr for client flows that have not yet received anything. */
    if(sock->last_src_valid) {
      peeraddr = &sock->last_src_addr;
    } else if(sock->socket.udp_conn != NULL) {
      peeraddr = &sock->socket.udp_conn->ripaddr;
    } else {
      return 0;
    }
  }

  return peer_addr_to_vector(thread, peeraddr, obj);
}

/*
 * Accept-client model on Contiki-NG:
 *
 *   A server vm_port_t owns one native_tcp_socket in listening mode
 *   (PENDING). When TCP_SOCKET_CONNECTED fires for that slot, its
 *   state flips to CONNECTED and incoming-client? returns #t.
 *
 *   accept-client then:
 *     1. detaches the connected slot from the server port,
 *     2. allocates a fresh native_tcp_socket and starts a new
 *        tcp_socket_listen on the same port for the server,
 *     3. allocates a new vm_port_t for the client and binds the
 *        connected slot to it.
 *
 *   This keeps memory linear in the number of concurrent clients
 *   (one MEMB slot per active connection plus one for the listener)
 *   and avoids needing to "move" a struct tcp_socket -- the
 *   already-registered slot just changes which port owns it.
 *
 *   While the listener is being replenished there is a tiny window
 *   where new SYNs hit a closed port, but tcp-socket.c does not
 *   queue connections so nothing is lost beyond what the OS would
 *   already drop.
 */
void
vm_native_accept_client(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  struct native_tcp_socket *connected;
  struct native_tcp_socket *listener;
  vm_port_t *client_port;
  vm_integer_t listen_port;

  if(port->io != &device_tcp) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    vm_set_error_string(thread, "accept-client requires a TCP server port");
    return;
  }

  connected = port->opaque_desc;
  if(connected == NULL) {
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, "server port not open");
    return;
  }

  if(connected->state != NATIVE_TCP_CONNECTED) {
    /* No client ready yet. Park the calling thread; the event
       callback will wake us when one arrives, or the polling timer
       will retry shortly. The eval loop re-enters accept-client on
       wake-up via VM_EXPR_RESTART. */
    vm_native_sleep(thread, VM_POLL_TIME);
    if(thread->expr != NULL) {
      VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    }
    return;
  }

  listen_port = connected->lport;

  /* Allocate a fresh listener for the server before we hand off the
     connected slot, so a failure leaves the server still listening
     instead of in a half-broken state. */
  listener = allocate_tcp_socket();
  if(listener == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    vm_set_error_string(thread, "TCP socket pool exhausted");
    return;
  }
  listener->port = port;
  if(!start_tcp_listen(listener, listen_port)) {
    vm_native_release_tcp_socket(listener);
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, "failed to relisten after accept");
    return;
  }

  /* Allocate the new client port. */
  client_port = vm_alloc(sizeof(vm_port_t));
  if(client_port == NULL) {
    /* Tear down the fresh listener we just brought up; we cannot
       complete the accept and we should not silently leak it. The
       server keeps its previous slot (still in CONNECTED state) so
       a retry can succeed. */
    tcp_socket_close(&listener->socket);
    tcp_socket_unregister(&listener->socket);
    vm_native_release_tcp_socket(listener);
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  vm_port_register(client_port);

  client_port->thread = thread;
  client_port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                       VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;
  client_port->io = &device_tcp;
  client_port->opaque_desc = connected;
  connected->port = client_port;

  /* The server port now owns the fresh listener. */
  port->opaque_desc = listener;

  obj->type = VM_TYPE_PORT;
  obj->value.port = client_port;

  LOG_INFO("Accepted TCP client on port %u\n", (unsigned)listen_port);
}

void
vm_native_incoming_clientp(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  struct native_tcp_socket *sock;

  obj->type = VM_TYPE_BOOLEAN;
  obj->value.boolean = VM_FALSE;

  if(port->io != &device_tcp) {
    return;
  }

  sock = port->opaque_desc;
  if(sock == NULL) {
    return;
  }

  if(sock->state == NATIVE_TCP_CONNECTED) {
    obj->value.boolean = VM_TRUE;
  }
}

int
vm_native_resolve(vm_thread_t *thread, const char *hostname)
{
  uip_ipaddr_t *ip_addr;
  resolv_status_t status;
  uint16_t req_cost;
#define DNS_PORT 53

  if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_DNS)) {
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return -1;
  }

  vm_policy_check_bandwidth(thread);

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    attribute_bandwidth(thread, 0);
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return 0;
  }

  attribute_communication(thread, UIP_PROTO_UDP, uip_ntohs(DNS_PORT));

  status = resolv_lookup(hostname, &ip_addr);
  switch(status) {
  case RESOLV_STATUS_CACHED:
    vm_vector_create(&thread->result, sizeof(uip_ipaddr_t),
                     VM_VECTOR_FLAG_REGULAR);
    ip_to_vector(ip_addr, thread->result.value.vector);
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return 1;
  case RESOLV_STATUS_UNCACHED:
  case RESOLV_STATUS_EXPIRED:
    /*
     * TODO: This is only a placeholder. We need an API in the OS to gain
     * information about bandwidth usage that is caused indirectly. Assume
     * that on average 1 request will be sent, and that the reply is three
     * times as large.
     */
    req_cost = 18 + strlen(hostname);
    req_cost *= 4;
    attribute_bandwidth(thread, req_cost);
    resolv_query(hostname);
    /* fall through */
  case RESOLV_STATUS_RESOLVING:
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return 0;
  default:
    /* An error occurred if we reach this point. */
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return -1;
  }
}

int
vm_native_parse_address(const char *str, uint8_t *bytes, size_t *len)
{
  uip_ipaddr_t addr;

  if(uiplib_ipaddrconv(str, &addr) != 0) {
    memcpy(bytes, addr.u8, sizeof(addr.u8));
    *len = sizeof(addr.u8);
    return 1;
  }
  return 0;
}

vm_port_t *
vm_native_open_file(vm_thread_t *thread, const char *filename, int direction)
{
  vm_port_t *port;
  vm_device_t *device;

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    return NULL;
  }
  vm_port_register(port);

  port->thread = thread;

  device = vm_device_lookup(filename);
  if(device == NULL) {
    port->io = &device_cfs;
    port->flags = VM_PORT_FLAG_FILE;
  } else {
    if((device->flags & direction) != direction) {
      vm_signal_error(thread, VM_ERROR_IO);
      vm_set_error_string(thread, filename);
      return NULL;
    }

    port->io = device->io;
    port->flags = VM_PORT_FLAG_DEVICE;
    filename = device->name;
    VM_DEBUG(VM_DEBUG_LOW, "Use device %s for I/O on file %s",
             device->name, filename);
  }

  port->fd = port->io->open == NULL ?
               0 : port->io->open(port, filename, direction);
  if(port->fd == -1) {
    vm_free(port);
    return NULL;
  }

  port->flags |= VM_PORT_FLAG_OPEN | direction;

  return port;
}

int
vm_native_read(vm_port_t *port, vm_obj_t *obj)
{
  int r;

  if(port->io && port->io->read_object) {
    r = port->io->read_object(port, obj);
  } else {
    r = -1;
  }

  if(r < 1) {
    if(r < 0) {
      vm_signal_error(port->thread, VM_ERROR_IO);
      vm_set_error_string(port->thread, "port read failed");
    }
    return 0;
  }

  return 1;
}

int
vm_native_read_char(vm_port_t *port, vm_character_t *c)
{
  char buf[1];
  int r;

  if(port->io && port->io->read) {
    r = port->io->read(port, buf, 1);
  } else {
    r = -1;
  }

  if(r < 1) {
    if(r < 0) {
      vm_signal_error(port->thread, VM_ERROR_IO);
      vm_set_error_string(port->thread, "port read failed");
    }
    return 0;
  }

  *c = buf[0];
  return 1;
}

int
vm_native_peek_char(vm_port_t *port, vm_character_t *c)
{
  return 0;
}

vm_boolean_t
vm_native_char_readyp(vm_port_t *port)
{
  return VM_TRUE;
}

int
vm_native_write(vm_port_t *port, const char *format, ...)
{
  va_list args;
  int len;
  char buf[VM_CONSOLE_BUFFER_SIZE];

  va_start(args, format);
  len = vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);

  return vm_native_write_buffer(port, buf, len);
}

int
vm_native_write_buffer(vm_port_t *port, const char *buf, size_t len)
{
  int ret;
  struct native_socket *sock;

  if(port == NULL) {
    port = vm_native_default_port(NULL, VM_PORT_FLAG_OUTPUT);
  }

  if(port != NULL && port->io != NULL && port->io->write != NULL) {
    ret = port->io->write(port, buf, len);
  } else {
    ret = printf("%s", buf);
  }

  if(port->thread != NULL) {
    if(ret < 0) {
      vm_signal_error(port->thread, VM_ERROR_IO);
      vm_set_error_string(port->thread, "port write failed");
    } else if(port != NULL && VM_IS_SET(port->flags, VM_PORT_FLAG_SOCKET)) {
      sock = port->opaque_desc;
      attribute_bandwidth(port->thread, len);
      vm_policy_check_bandwidth(port->thread);
      attribute_communication(port->thread, sock->proto,
                              VM_MIN(uip_ntohs(sock->lport), uip_ntohs(sock->rport)));
    }
  }

  return ret;
}

char *
vm_native_strdup(const char *src)
{
  char *dst;
  size_t size;

  size = strlen(src) + 1;
  dst = VM_MALLOC(size);
  if(dst == NULL) {
    return NULL;
  }
  return memcpy(dst, src, size);
}

/* Returns the power consumption of a program, expressed in microwatts. */
unsigned
vm_native_calculate_power(const vm_program_t *program)
{
  uint64_t power; /* Expressed in mW. */
  uint32_t uptime; /* Expressed in seconds. */

  /* The program consumes CPU power and radio power. LPM power consumption
     belongs to the system, and is thus not included here. */
  power = ((uint64_t)program->perf_attr.cpu_time * 1800);
  power += ((uint64_t)program->perf_attr.radio_rx_time * 20000);
  power += ((uint64_t)program->perf_attr.radio_tx_time * 17700);

  power = 3 * (power / RTIMER_SECOND); /* 3 volts for TMote Sky devices. */

  uptime = clock_seconds();
  /* Avoid division by zero. */
  if(uptime == 0) {
    uptime++;
  }
  power /= uptime;

  VM_DEBUG(VM_DEBUG_MEDIUM, "Power: %lu uW\n", (unsigned long)power);

  return power;
}

void
vm_native_accounting_start(vm_thread_t *thread)
{
  attribute_energy(thread, 0);
}

void
vm_native_accounting_stop(vm_thread_t *thread)
{
  attribute_energy(thread, 1);
}
