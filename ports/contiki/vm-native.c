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
#include "lib/list.h"
#include "lib/memb.h"
#include "lib/sensors.h"
#include "net/ip/uip.h"
#include "net/ip/udp-socket.h"
#include "net/ip/uip-nameserver.h"
#include "net/ip/resolv.h"
#include "net/ipv6/uip-ds6.h"
#include "sys/energest.h"
#if WITH_POWERTRACE
#include "powertrace.h"
#endif

#define DEBUG DEBUG_PRINT
#include "net/ip/uip-debug.h"

#include "vm-log.h"
#include "vm-native.h"

PROCESS_NAME(vm_process);

extern const vm_port_io_t device_cfs;
extern const vm_port_io_t device_leds;
extern const vm_port_io_t device_sensors;
extern const vm_port_io_t device_serial;
extern const vm_port_io_t device_uip;

static struct ctimer timers[VM_THREAD_AMOUNT];

#ifndef VM_MAX_SOCKETS
#define VM_MAX_SOCKETS 2
#endif

MEMB(socket_memb, struct native_socket, VM_MAX_SOCKETS);
LIST(socket_list);

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
    list_push(socket_list, sock);
  }
  return sock;
}

static void
free_socket(struct native_socket *sock)
{
  list_remove(socket_list, sock);
  memb_free(&socket_memb, sock);
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
#if WITH_POWERTRACE
  struct powertrace_sniff_stats *stats;

  VM_DEBUG(VM_DEBUG_MEDIUM, "Attribute communication for proto %u, port %u",
           (unsigned)proto, (unsigned)port);

  stats = powertrace_get_channel_stats(proto, port);
  if(stats == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to get stats for proto %u, port %u",
             (unsigned)proto, (unsigned)port);
    return;
  }

  VM_DEBUG(VM_DEBUG_MEDIUM,
           "UDP port %u: TXin %lu TXout %lu RXin %lu RXout %lu",
           (unsigned)port,
           (unsigned long)stats->input_txtime,
           (unsigned long)stats->output_txtime,
           (unsigned long)stats->input_rxtime,
           (unsigned long)stats->output_rxtime);
  thread->program->perf_attr.radio_rx_time = stats->input_rxtime +
                                             stats->output_rxtime;
  thread->program->perf_attr.radio_tx_time = stats->input_txtime +
                                             stats->output_txtime;
#endif
}

static void
udp_input(struct udp_socket *sock, void *ptr,
          const uip_ipaddr_t *source_addr, uint16_t source_port,
          const uip_ipaddr_t *dest_addr, uint16_t dest_port,
          const uint8_t *data, uint16_t datalen)
{
  vm_thread_t *thread;

  thread = ptr;

  attribute_communication(thread, UIP_PROTO_UDP,
                          VM_MIN(uip_ntohs(source_port), uip_ntohs(dest_port)));

  VM_DEBUG(VM_DEBUG_MEDIUM,
           "Program %s received a UDP packet", thread->program->name);
}

static void
attribute_energy(vm_thread_t *thread, int done)
{
  static unsigned long e_cpu, e_lpm;
  unsigned long diff_cpu, diff_lpm;

  if(!done) {
    e_cpu      = energest_type_time(ENERGEST_TYPE_CPU);
    e_lpm      = energest_type_time(ENERGEST_TYPE_LPM);
  } else {
    diff_cpu      = energest_type_time(ENERGEST_TYPE_CPU) - e_cpu;
    diff_lpm      = energest_type_time(ENERGEST_TYPE_LPM) - e_lpm;

    thread->program->perf_attr.cpu_time += diff_cpu;

    vm_policy_check_cpu(thread->program,
                        (100 * (unsigned long)thread->program->perf_attr.cpu_time) /
                        (diff_cpu + e_cpu + diff_lpm + e_lpm));

    vm_policy_check_power(thread);
  }
}

static void
register_devices(void)
{
  const struct sensors_sensor *sensor;

  /* Register the Contiki File System device. */
  if(vm_device_register("cfs",
                        &device_cfs, VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to register %s", "leds");
  }

  /* Register the LEDs device. */
  if(vm_device_register("leds",
                        &device_leds, VM_PORT_FLAG_OUTPUT) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to register %s", "leds");
  }

  /* Register all sensors available in Contiki. */
  sensor = sensors_first();
  do {
    VM_DEBUG(VM_DEBUG_LOW, "Register sensor %s", sensor->type);
    if(vm_device_register(sensor->type, &device_sensors,
                          VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT) == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Failed to register %s", sensor->type);
    }
  } while((sensor = sensors_next(sensor)) != NULL);

  /* Register the serial device. */
  if(vm_device_register("serial",
                        &device_serial, VM_PORT_FLAG_INPUT) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to register %s", "serial");
  }
}

int
vm_native_init(void)
{
  uip_ipaddr_t dns_addr;

  register_devices();

  /* Temporary hack to use a nameserver at the border router's host. */
  uip_ip6addr(&dns_addr, UIP_DS6_DEFAULT_PREFIX, 0, 0, 0, 0, 0, 0, 1);
  uip_nameserver_update(&dns_addr, 1000000);

#if WITH_POWERTRACE
  VM_DEBUG(VM_DEBUG_LOW, "Starting the Powertrace sniffer");
  powertrace_sniff(POWERTRACE_ON);
#endif

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
  return CONTIKI_VERSION_STRING;
}

void
vm_native_poll(void)
{
}

void
vm_native_sleep(vm_thread_t *thread, vm_integer_t ms)
{
  vm_id_index_t index;

  index = vm_thread_get_index(thread);
  if(index == VM_ID_INDEX_INVALID) {
    vm_signal_error(thread, VM_ERROR_INTERNAL);
  } else {
    ctimer_set(&timers[index], (CLOCK_SECOND * ms) / 1000,
               thread_timer_expired, thread);
    thread->status = VM_THREAD_WAITING;
  }
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
  if(port->io && port->io->close) {
    port->io->close(port);
  }

  if(IS_SET(port->flags, VM_PORT_FLAG_SOCKET)) {
    free_socket(port->opaque_desc);
  }

  port->flags &= ~VM_PORT_FLAG_OPEN;
}

vm_port_t *
vm_native_default_port(vm_thread_t *thread, int direction)
{
  return &serial_port;
}

vm_port_t *
vm_native_open_client(vm_thread_t *thread, vm_socket_type_t socket_type,
                      vm_vector_t *address, vm_integer_t dest_port,
                      vm_integer_t source_port)
{
  struct native_socket *sock;
  uip_ipaddr_t addr;
  vm_port_t *port;

  if(vector_to_ip(address, &addr) == 0) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to convert an IP address");
    return NULL;
  }

  sock = allocate_socket();
  if(sock == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to allocate a socket");
    return NULL;
  }

  if(udp_socket_register(&sock->socket, thread, udp_input) < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to create a UDP socket");
    free_socket(sock);
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

  if(udp_socket_connect(&sock->socket, &addr, dest_port) < 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to connect a UDP socket");
    udp_socket_close(&sock->socket);
    free_socket(sock);
    return NULL;
  }

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    udp_socket_close(&sock->socket);
    free_socket(sock);
    return NULL;
  }

  port->thread = thread;
  port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;
  port->io = &device_uip;
  port->opaque_desc = sock;

#if VM_DEBUG_LEVEL >= VM_DEBUG_LOW
  VM_PRINTF("VM: Created a UDP connection to ");
  PRINT6ADDR(&addr);
  VM_PRINTF("\n");
#endif

  return port;
}

vm_port_t *
vm_native_open_server(vm_thread_t *thread,
                   vm_vector_t *address,
                   vm_integer_t listen_port)
{
  return NULL;
}

int
vm_native_get_peer_name(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  struct native_socket *sock;
  vm_vector_t *vector;
  vm_obj_t item;
  int i;
  uip_ipaddr_t *peeraddr;

  sock = port->opaque_desc;
  peeraddr = &sock->socket.udp_conn->ripaddr;

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

void
vm_native_accept_client(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
}

void
vm_native_incoming_clientp(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
}

int
vm_native_resolve(vm_thread_t *thread, const char *hostname)
{
  uip_ipaddr_t *ip_addr;
  resolv_status_t status;
  uint16_t req_cost;
#define DNS_PORT 53

  if(!vm_policy_check_resources(thread, VM_POLICY_RESOURCE_DNS)) {
    CLEAR(thread->expr->flags, VM_EXPR_RESTART);
    return -1;
  }

  vm_policy_check_bandwidth(thread);

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    attribute_bandwidth(thread, 0);
    vm_native_sleep(thread, VM_POLL_TIME);
    SET(thread->expr->flags, VM_EXPR_RESTART);
    return 0;
  }

  attribute_communication(thread, UIP_PROTO_UDP, uip_ntohs(DNS_PORT));

  status = resolv_lookup(hostname, &ip_addr);
  switch(status) {
  case RESOLV_STATUS_CACHED:
    vm_vector_create(&thread->result, sizeof(uip_ipaddr_t),
                     VM_VECTOR_FLAG_REGULAR);
    ip_to_vector(ip_addr, thread->result.value.vector);
    CLEAR(thread->expr->flags, VM_EXPR_RESTART);
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
  case RESOLV_STATUS_RESOLVING:
    vm_native_sleep(thread, VM_POLL_TIME);
    SET(thread->expr->flags, VM_EXPR_RESTART);
    return 0;
  default:
    /* An error occurred if we reach this point. */
    CLEAR(thread->expr->flags, VM_EXPR_RESTART);
    return -1;
  }
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
    } else if(port != NULL && IS_SET(port->flags, VM_PORT_FLAG_SOCKET)) {
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
