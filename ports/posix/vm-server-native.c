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
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/time.h>

#include "vm.h"
#include "vm-control.h"
#include "vm-log.h"
#include "vm-native.h"

#if VM_SERVER

#include "lwm2m/coap-ipv4/coap-ipv4.h"
#include "sys/ntimer.h"

static int vm_server_fd = -1;

static const struct select_callback *coap_callback;

static void
ntimer_callback(ntimer_t *ntp)
{
}

int
select_set_callback(int fd, const struct select_callback *callback)
{
  VM_DEBUG(VM_DEBUG_LOW, "LWM2M fd = %d", fd);
  vm_posix_poll_port(NULL, fd, 0);
  vm_server_fd = fd;
  coap_callback = callback;

  return 1;
}

int
vm_server_init(void)
{
  static ntimer_t nt;

  ntimer_init();

  VM_DEBUG(VM_DEBUG_LOW, "Starting an LWM2M server");

  ntimer_set_callback(&nt, ntimer_callback);
  ntimer_set(&nt, 10000);

  vm_control_init();

  return 1;
}

void
vm_server_shutdown(void)
{
  vm_posix_unpoll_port(vm_server_fd);
  close(vm_server_fd);
}

void
vm_server_close(vm_port_t *port)
{
  vm_posix_unpoll_port(port->fd);
  close(port->fd);
}

void
vm_server_update(void)
{
#define NTIMER_LOOP_LIMIT 100
  int i;
  uint64_t next_ntimer_time;
  uint64_t next_alarm_time;
  struct itimerval it;

  for(i = 0; i < NTIMER_LOOP_LIMIT && ntimer_run() > 0; i++);

  next_ntimer_time = ntimer_time_to_next_expiration();

  if(next_ntimer_time > 0) {
    if(getitimer(ITIMER_REAL, &it) < 0) {
      vm_posix_syscall_error(NULL, "getitimer");
      return;
    }

    next_alarm_time = (it.it_value.tv_sec * 1000000 + it.it_value.tv_usec) / 1000;
    VM_DEBUG(VM_DEBUG_HIGH, "next_alarm_time %lu next_ntimer_time %lu",
             (unsigned long)next_alarm_time, (unsigned long)next_ntimer_time);

    if(next_ntimer_time < next_alarm_time) {
      /*
       * An ntimer has been set at a closer time than the currently set POSIX ITIMER_REAL,
       * so we schedule an alarm to be triggered when this timer expires.
       *
       * Both ntimer and VM timer resolutions are in milliseconds an
       * represented as 64-bit unsigned integers, so the ntimer value
       * can be casted to vm_integer_t.
       */
      vm_native_sleep(NULL, next_ntimer_time);
    }
  }
}

void
vm_server_incoming(int fd)
{
  fd_set read_fds;
  fd_set write_fds;

  VM_DEBUG(VM_DEBUG_LOW, "Incoming CoAP packet on fd %d", fd);

  FD_ZERO(&read_fds);
  FD_ZERO(&write_fds);
  FD_SET(fd, &read_fds);

  if(coap_callback != NULL) {
    coap_callback->handle_fd(&read_fds, &write_fds);
  }
}

#endif /* VM_SERVER */
