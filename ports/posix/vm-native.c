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

#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <poll.h>
#include <unistd.h>

#include "vm.h"
#include "vm-control.h"
#include "vm-log.h"
#include "vm-native.h"

extern const vm_port_io_t device_fs;
extern const vm_port_io_t device_zero;

static volatile sig_atomic_t timers_expired;
static uint16_t waiting_ports;
static uint16_t sleeping_threads;
static int ready_port_set[VM_PORT_AMOUNT];
static vm_thread_t *poll_thread_map[VM_PORT_AMOUNT];
static vm_port_t *port_map[VM_PORT_AMOUNT];
static struct pollfd poll_fd_set[VM_PORT_AMOUNT];

struct vm_timer {
  struct vm_timer *next;
  vm_thread_t *thread;
  uint64_t wakeup_time;
};

static struct vm_timer *timers;

static vm_port_t input_port = {.fd = STDIN_FILENO,
                               .io = &device_fs,
                               .flags = VM_PORT_FLAG_OPEN    |
                                        VM_PORT_FLAG_CONSOLE |
                                        VM_PORT_FLAG_INPUT};
static vm_port_t output_port = {.fd = STDOUT_FILENO,
                                .io = &device_fs,
                                .flags = VM_PORT_FLAG_OPEN    |
                                         VM_PORT_FLAG_CONSOLE |
                                         VM_PORT_FLAG_OUTPUT};

void
vm_posix_syscall_error(vm_thread_t *thread, const char *syscall_str)
{
  char buf[BUFSIZ];

  if(thread != NULL) {
    vm_signal_error(thread, VM_ERROR_SYSCALL);
    snprintf(buf, sizeof(buf), "%s() => \"%s\"",
	     syscall_str, strerror(errno));
    vm_set_error_string(thread, buf);
  } else {
    perror(syscall_str);
  }
}

static int
vector_to_ip(vm_vector_t *vector, in_addr_t *addr)
{
  int i;

  if(vector->length != sizeof(in_addr_t)) {
    return 0;
  }

  *addr = 0;
  for(i = vector->length - 1; i >= 0; i--) {
    if(vector->elements[i].type != VM_TYPE_INTEGER) {
      VM_DEBUG(VM_DEBUG_LOW, "Invalid element on position %d", i);
      return 0;
    }
    *addr <<= 8;
    *addr |= (vector->elements[i].value.integer & 0xff);
  }

  return vector->length;
}

static int
port_is_ready(int fd, int writable)
{
  if(fd >= VM_PORT_AMOUNT) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Checking port with too high fd: %d", fd);
    return 0;
  }

  return IS_SET(ready_port_set[fd], (writable ? POLLOUT : POLLIN));
}

int
vm_posix_poll_port(vm_thread_t *thread, int fd, int writable)
{
  if(waiting_ports == VM_PORT_AMOUNT) {
    return 0;
  }

  if(thread) {
    VM_DEBUG(VM_DEBUG_HIGH, "Thread %lu is polling fd %d",
             (unsigned long)thread->id, fd);
  }

  poll_thread_map[waiting_ports] = thread;

  poll_fd_set[waiting_ports].fd = fd;
  poll_fd_set[waiting_ports].events = writable ? POLLOUT : POLLIN;

  waiting_ports++;

  if(thread != NULL) {
    thread->status = VM_THREAD_WAITING;
  }

  return 1;
}

static vm_boolean_t
poll_port_instantly(int fd, int writable)
{
  struct pollfd fd_set[1];
  int ready;

  fd_set[0].fd = fd;
  fd_set[0].events = writable ? POLLOUT : POLLIN;

  ready = poll(fd_set, 1, 0);
  if(ready < 0) {
    if(errno != EINTR) {
      perror("poll");
    }
  } else if(ready == 1) {
    if(IS_SET(fd_set[0].revents, writable ? POLLOUT : POLLIN)) {
      return VM_TRUE;
    }
  }

  return VM_FALSE;
}

void
vm_posix_unpoll_port(int port_index)
{
  /* The thread map has a null entry if the VM itself is polling a port. */
  if(poll_thread_map[port_index] != NULL) {
    poll_thread_map[port_index]->status = VM_THREAD_RUNNABLE;
    poll_thread_map[port_index] = NULL;
  }

  poll_fd_set[port_index].events = 0;
  if(port_index == waiting_ports - 1) {
    waiting_ports--;
  }
}

static void
schedule_next_timer(uint64_t current_time)
{
  struct itimerval tv;
  struct vm_timer *timer;
  struct vm_timer *tmp_timer;

  for(timer = timers; timer != NULL;) {
    if(timer->wakeup_time < current_time) {
      if(timer->thread != NULL) {
	timer->thread->status = VM_THREAD_RUNNABLE;
      }
      tmp_timer = timer->next;
      VM_FREE(timer);
      timers = timer = tmp_timer;
    } else {
      timers = timer;
      break;
    }
  }

  if(timers == NULL) {
    /* No timers remain. */
    return;
  }

  /* Now, set the system timer to the interval specified by the timer with
     the lowest wake-up time; i.e., the one that is first in the list. */

  tv.it_value.tv_sec = (timers->wakeup_time - current_time) / 1000;
  tv.it_value.tv_usec = ((timers->wakeup_time - current_time) % 1000) * 1000;

  /* This is a one-shot timer. */
  tv.it_interval.tv_sec = 0;
  tv.it_interval.tv_usec = 0;

  if(setitimer(ITIMER_REAL, &tv, NULL) < 0) {
    vm_posix_syscall_error(timers->thread, "setitimer");
  }

  VM_DEBUG(VM_DEBUG_HIGH, "Scheduling a timer set at %lu ms", (unsigned long)current_time);
}

static int
add_timer(vm_thread_t *thread, unsigned long sleep_msec)
{
  struct vm_timer *timer;
  struct vm_timer *iter;
  vm_time_t current_time;
  uint64_t time_msec;

  if(!vm_native_time(&current_time)) {
    return 0;
  }

  time_msec = (uint64_t)current_time.sec * 1000 + current_time.msec;

  timer = VM_MALLOC(sizeof(struct vm_timer));
  if(timer == NULL) {
    return 0;
  }

  timer->thread = thread;
  timer->wakeup_time = time_msec + sleep_msec;

  if(timers == NULL) {
    timers = timer;
    timer->next = NULL;
  } else {
    for(iter = timers; iter != NULL; iter = iter->next) {
      if(timer->wakeup_time < iter->wakeup_time) {
        /* Place the timer before another timer. */
        sleeping_threads++;
        if(iter == timers) {
          /* Place the timer first in the list. */
          timer->next = timers;
          timers = timer;
        } else {
          /* Place the timer somewhere between two other timers in the list. */
          timer->next = iter;

          /* Find the timer that should be placed before the added timer. */
          iter = timers;
          while(iter->next != timer->next) {
            iter = iter->next;
          }
          iter->next = timer;
        }
        break;
      }

      if(iter->next == NULL) {
        /* Place the timer last in the list. */
        sleeping_threads++;
        iter->next = timer;
        timer->next = NULL;
        break;
      }
    }
  }

#if 0
  VM_PRINTF("Timer list: ");
  for(iter = timers; iter != NULL; iter = iter->next) {
    VM_PRINTF("%s (%lu ms), ", iter->thread->program->name,
              (unsigned long)(iter->wakeup_time - current_time));
  }
  VM_PRINTF("\n");
#endif

  schedule_next_timer(time_msec);

  return 1;
}

static void
process_timers(void)
{
  vm_time_t current_time;
  uint64_t time_msec;
  struct vm_timer *timer;
  struct vm_timer *tmp_timer;

  if(!vm_native_time(&current_time)) {
    VM_DEBUG(VM_DEBUG_LOW, "Unable to get the current time");
    timers_expired = 0;
    return;
  }

  time_msec = (uint64_t)current_time.sec * 1000 + current_time.msec;

  for(timer = timers; timer != NULL && timer->wakeup_time <= time_msec;) {
    sleeping_threads--;

    if(timer->thread != NULL) {
      timer->thread->status = VM_THREAD_RUNNABLE;
    }
    tmp_timer = timer->next;
    VM_DEBUG(VM_DEBUG_HIGH, "Remove timer (expiration %ld)",
              (long)((int64_t)timer->wakeup_time - (int64_t)time_msec));
    VM_FREE(timer);
    timer = tmp_timer;
  }

  timers = timer;
  if(timer != NULL) {
    schedule_next_timer(time_msec);
  }

  timers_expired = 0;
}

static void
alarm_handler(int signo)
{
  timers_expired = 1;

  if(signal(SIGALRM, alarm_handler) == SIG_ERR) {
    perror("signal");
  }
}

int
vm_native_init(void)
{
  if(signal(SIGALRM, alarm_handler) == SIG_ERR ||
     signal(SIGPIPE, SIG_IGN) == SIG_ERR) {
    perror("signal");
    return 0;
  }

  if(fcntl(0, F_SETFD, O_NONBLOCK) < 0) {
    perror("fcntl");
    return 0;
  }

  if(vm_device_register("zero", &device_zero, VM_PORT_FLAG_INPUT) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to register /dev/zero");
    return 0;
  }

  srandom((unsigned)time(NULL));

  if(!vm_control_init()) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to initialize the VM control interface");
    return 0;
  }

  return 1;
}

const char *
vm_native_get_os_version(void)
{
  static char os_version[256];
  struct utsname name;

  /* Set the os_version string once only. */
  if(os_version[0] == '\0') {
    if(uname(&name) < 0) {
      snprintf(os_version, sizeof(os_version), "<unknown OS>");
    } else {
      snprintf(os_version, sizeof(os_version), "%s %s (%s)",
               name.sysname, name.release, name.machine);
    }
  }

  return os_version;
}

void
vm_native_poll(void)
{
  int poll_time;
  int i;
  int ready;

  if(timers_expired) {
    process_timers();
  }

  /* Sleep 0 ms by default. */
  poll_time = 0;
  if(vm_get_programs() != NULL && vm_thread_running() == 0) {
    /* If all threads are sleeping, poll for an infinite amount of time. */
    poll_time = -1;
  } else if(sleeping_threads == 0 && waiting_ports == 0) {
    return;
  }

  VM_DEBUG(VM_DEBUG_HIGH, "Poll time: %d", poll_time);

  ready = poll(poll_fd_set, waiting_ports, poll_time);
  if(ready < 0) {
    if(errno != EINTR) {
      perror("poll");
    }
  } else if(ready > 0) {
    for(i = 0; i < waiting_ports; i++) {
      if(IS_SET(poll_fd_set[i].revents, POLLIN | POLLOUT)) {
        ready--;
        SET(ready_port_set[poll_fd_set[i].fd], poll_fd_set[i].revents);
        if(poll_thread_map[i] == NULL) {
#if VM_SERVER
          vm_server_incoming(poll_fd_set[i].fd);
#endif
        } else {
          vm_posix_unpoll_port(i);
        }
      }
    }
  }
}

void
vm_native_sleep(vm_thread_t *thread, vm_integer_t ms)
{
  VM_DEBUG(VM_DEBUG_HIGH, "Sleep %lu ms", (unsigned long)ms);

  if(thread != NULL) {
    thread->status = VM_THREAD_WAITING;
  }
  add_timer(thread, ms);
}

int
vm_native_time(vm_time_t *current_time)
{
  struct timeval tv;
  vm_thread_t *thread;

  if(gettimeofday(&tv, NULL) < 0) {
    thread = vm_current_thread();
    if(thread != NULL) {
      vm_signal_error(thread, VM_ERROR_INTERNAL);
      return 0;
    }
  }

  current_time->sec = tv.tv_sec;
  current_time->msec = tv.tv_usec / 1000;

  return 1;
}

void
vm_native_close_port(vm_port_t *port)
{
  if(port->io != NULL && port->io->close != NULL) {
    port->io->close(port);
  } else {
    close(port->fd);
  }
  CLEAR(port->flags, VM_PORT_FLAG_OPEN);
}

vm_port_t *
vm_native_default_port(vm_thread_t *thread, int direction)
{
  switch(direction) {
  case VM_PORT_FLAG_INPUT:
    return &input_port;
  case VM_PORT_FLAG_OUTPUT:
    return &output_port;
  default:
    return NULL;
  }
}

vm_port_t *
vm_native_open_client(vm_thread_t *thread, vm_socket_type_t socket_type,
                      vm_vector_t *address, vm_integer_t dest_port,
                      vm_integer_t source_port)
{
  vm_port_t *port;
  struct sockaddr_in sa;

  /* TODO: Handle the source port argument. */

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    return NULL;
  }

  memset(&sa, 0, sizeof(struct sockaddr_in));
  sa.sin_family = AF_INET;
  sa.sin_port = htons(dest_port);
  if(vector_to_ip(address, &sa.sin_addr.s_addr) == 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_string(thread, "invalid IP address");
    return NULL;
  }

  VM_DEBUG(VM_DEBUG_MEDIUM, "Open socket to %s:%u",
	    inet_ntoa(sa.sin_addr), (unsigned)dest_port);

  port->thread = thread;
  port->io = &device_fs;
  port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;

  port->fd = socket(PF_INET,
                    socket_type == VM_SOCKET_STREAM ? SOCK_STREAM :
                                                      SOCK_DGRAM,
                    0);
  if(port->fd < 0) {
    vm_posix_syscall_error(thread, "socket");
    vm_free(port);
    return NULL;
  } else if(port->fd >= VM_PORT_AMOUNT) {
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, "out of ports");
    close(port->fd);
    vm_free(port);
    return NULL;
  }

  if(connect(port->fd, (struct sockaddr *)&sa,
	     sizeof(struct sockaddr_in)) < 0) {
    vm_posix_syscall_error(thread, "connect");
    close(port->fd);
    vm_free(port);
    return NULL;
  }

  port_map[port->fd] = port;

  return port;
}

vm_port_t *
vm_native_open_server(vm_thread_t *thread,
		   vm_vector_t *address,
		   vm_integer_t listen_port)
{
  vm_port_t *port;
  struct sockaddr_in sa;
  int flags;

  port = vm_alloc(sizeof(vm_port_t));
  if(port == NULL) {
    return NULL;
  }

  port->thread = thread;
  port->io = &device_fs;
  port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;

  memset(&sa, 0, sizeof(sa));
  sa.sin_family = AF_INET;
  sa.sin_port = htons(listen_port);
#if 0
  if(vector_to_ip(address, &sa.sin_addr.s_addr) == 0) {
    return NULL;
  }
#endif

  VM_DEBUG(VM_DEBUG_MEDIUM, "Bind address %s:%u",
	    inet_ntoa(sa.sin_addr), (unsigned)listen_port);

  /* TODO: Handle UDP sockets differently. */
  port->fd = socket(PF_INET, SOCK_STREAM, 0);
  if(port->fd == -1) {
    vm_posix_syscall_error(thread, "socket");
    vm_free(port);
    return NULL;
  } else if(port->fd >= VM_PORT_AMOUNT) {
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, "out of ports");
    close(port->fd);
    vm_free(port);
    return NULL;
  }

  if(bind(port->fd, (struct sockaddr *)&sa, sizeof(struct sockaddr_in)) < 0) {
    vm_posix_syscall_error(thread, "bind");
    close(port->fd);
    vm_free(port);
    return NULL;
  }

  /* Listen only if it is a TCP socket. */
  if(listen(port->fd, VM_POSIX_LISTEN_SLOTS) < 0) {
    vm_posix_syscall_error(thread, "listen");
    close(port->fd);
    vm_free(port);
    return NULL;
  }

  flags = fcntl(port->fd, F_GETFL, 0);
  if(flags < 0) {
    vm_posix_syscall_error(thread, "fcntl");
    close(port->fd);
    return NULL;
  }

  if(fcntl(port->fd, F_SETFL, flags | O_NONBLOCK) < 0) {
    vm_posix_syscall_error(thread, "fcntl");
    close(port->fd);
    return NULL;
  }

  port_map[port->fd] = port;

  return port;
}

int
vm_native_get_peer_name(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  struct sockaddr_in sa;
  socklen_t addr_len;
  vm_vector_t *vector;
  vm_obj_t item;
  unsigned i;

  if(IS_CLEAR(port->flags, VM_PORT_FLAG_SOCKET)) {
    vm_signal_error(thread, VM_ERROR_IO);
    return 0;
  }

  addr_len = sizeof(sa);
  if(getpeername(port->fd, (struct sockaddr *)&sa, &addr_len) < 0) {
    vm_posix_syscall_error(thread, "getpeername");
    return 0;
  }

  vector = vm_vector_create(obj, sizeof(sa.sin_addr), VM_VECTOR_FLAG_REGULAR);
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return 0;
  }

  item.type = VM_TYPE_INTEGER;
  for(i = 0; i < sizeof(sa.sin_addr); i++) {
    item.value.integer = (sa.sin_addr.s_addr >> (i * 8)) & 0xff;
    vm_vector_set(obj, i, &item);
  }

  return 1;
}

void
vm_native_accept_client(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  vm_port_t *new_port;

  new_port = vm_alloc(sizeof(vm_port_t));
  if(new_port == NULL) {
    return;
  }

  new_port->thread = thread;
  new_port->io = &device_fs;
  new_port->flags = VM_PORT_FLAG_OPEN | VM_PORT_FLAG_SOCKET |
                    VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OUTPUT;

  new_port->fd = accept(port->fd, NULL, NULL);
  if(new_port->fd < 0) {
    vm_posix_syscall_error(thread, "accept");
  } else if(new_port->fd >= VM_PORT_AMOUNT) {
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, "out of ports");
    close(new_port->fd);
    vm_free(new_port);
  } else {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Accepted a new client on fd %d", port->fd);
    VM_PUSH_PORT(new_port);
    port_map[new_port->fd] = new_port;
  }
}

void
vm_native_incoming_clientp(vm_thread_t *thread, vm_port_t *port, vm_obj_t *obj)
{
  obj->type = VM_TYPE_BOOLEAN;
  obj->value.boolean = poll_port_instantly(port->fd, 0);
}

int
vm_native_resolve(vm_thread_t *thread, const char *hostname)
{
  return -1;
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

  device = vm_device_lookup(filename);
  if(device == NULL) {
    port->io = &device_fs;
    port->flags = VM_PORT_FLAG_FILE;
  } else {
    if((device->flags & direction) != direction) {
      vm_signal_error(thread, VM_ERROR_IO);
      return NULL;
    }

    port->thread = thread;
    port->io = device->io;
    port->flags = VM_PORT_FLAG_DEVICE;
    filename = device->name;
    VM_DEBUG(VM_DEBUG_MEDIUM, "Use device %s for I/O on file %s",
             device->name, filename);
  }

  port->fd = port->io->open == NULL ?
               0 : port->io->open(port, filename, direction);
  if(port->fd == -1) {
    vm_free(port);
    return NULL;
  }

  port->flags |= VM_PORT_FLAG_OPEN | direction;
  port_map[port->fd] = port;

  return port;
}

int
vm_native_read(vm_port_t *port, vm_obj_t *obj)
{
  char buf[8192]; /* Temporary test buffer. */
  ssize_t len;
  vm_vector_t *vector;

  if(port->io == NULL || port->io->read == NULL) {
    vm_signal_error(port->thread, VM_ERROR_IO);
    return 0;
  }

  if(port_is_ready(port->fd, 0)) {
    CLEAR(ready_port_set[port->fd], ~POLLIN);

    len = port->io->read(port, buf, sizeof(buf));
    if(len < 0) {
      vm_signal_error(port->thread, VM_ERROR_IO);
      vm_set_error_string(port->thread, strerror(errno));
      return 0;
    } else if(len == 0) {
      return 0;
    }

    vector = vm_vector_create(obj, len, VM_VECTOR_FLAG_BUFFER) ;
    if(vector == NULL) {
      vm_signal_error(port->thread, VM_ERROR_HEAP);
      return 0;
    }

    memcpy(vector->bytes, buf, len);
    return 1;
  } else {
    if(vm_posix_poll_port(port->thread, port->fd, 0) == 0) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "native: failed to poll fd %d", port->fd);
      return 0;
    }
    return 1;
  }

  return 0;
}

int
vm_native_read_char(vm_port_t *port, vm_character_t *c)
{
  char buf[1];
  int r;

  if(port->io == NULL || port->io->read == NULL) {
    vm_signal_error(port->thread, VM_ERROR_IO);
    return 0;
  }

  if(port_is_ready(port->fd, 0)) {
    CLEAR(ready_port_set[port->fd], ~POLLIN);
    r = port->io->read(port, buf, 1);
    if(r < 0) {
      vm_signal_error(port->thread, VM_ERROR_IO);
      vm_set_error_string(port->thread, strerror(errno));
      return 0;
    } else if(r == 0) {
      return 0;
    }

    *c = buf[0];
    return 1;
  }

  if(vm_posix_poll_port(port->thread, port->fd, 0) == 0) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "native: failed to poll fd %d", port->fd);
    return 0;
  }

  return 0;
}

int
vm_native_peek_char(vm_port_t *port, vm_character_t *c)
{
  return 0;
}

vm_boolean_t
vm_native_char_readyp(vm_port_t *port)
{
  return poll_port_instantly(port->fd, 0);
}

int
vm_native_write(vm_port_t *port, const char *format, ...)
{
  va_list args;
  int ret;

  if(port == NULL) {
    port = vm_native_default_port(NULL, VM_PORT_FLAG_OUTPUT);
  }

  va_start(args, format);

  ret = vdprintf(port == NULL ? STDOUT_FILENO : port->fd, format, args);

  va_end(args);

  if(ret < 0) {
    vm_signal_error(port->thread, VM_ERROR_IO);
    vm_set_error_string(port->thread, strerror(errno));
  }

  return ret;
}

int
vm_native_write_buffer(vm_port_t *port, const char *buf, size_t len)
{
  int ret;

  if(port == NULL) {
    port = vm_native_default_port(NULL, VM_PORT_FLAG_OUTPUT);
  }

  ret = write(port->fd, buf, len);

  if(ret < (int)len) {
    vm_signal_error(port->thread, VM_ERROR_IO);
    if(ret < 0) {
      vm_set_error_string(port->thread, strerror(errno));
    }
  }

  return ret;
}

unsigned
vm_native_calculate_power(const vm_program_t *program)
{
  return 0;
}

void
vm_native_accounting_start(vm_thread_t *thread)
{
}

void
vm_native_accounting_stop(vm_thread_t *thread)
{
}
