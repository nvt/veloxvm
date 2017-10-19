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

#include <stdio.h>

#include "contiki.h"
#include "dev/serial-line.h"

#include "vm.h"
#include "vm-log.h"

#if CONTIKI_TARGET_ZOUL
extern ssize_t write(int, const void *, size_t);
#endif

static char serial_buf[VM_CONSOLE_BUFFER_SIZE]; /* Temporary test buffer. */
/*---------------------------------------------------------------------------*/
PROCESS(vm_serial_process, "VM serial device");
/*---------------------------------------------------------------------------*/
PROCESS_THREAD(vm_serial_process, ev, data)
{
  PROCESS_BEGIN();

  while(1) {
    PROCESS_WAIT_EVENT_UNTIL(serial_line_event_message);
    VM_PRINTF("Serial input: %s\n", (char *)data);
    strncpy(serial_buf, data, sizeof(serial_buf) - 1);
    serial_buf[sizeof(serial_buf) - 1] = '\0';
  }

  PROCESS_END();
}
/*---------------------------------------------------------------------------*/
static int
serial_open(vm_port_t *port, const char *filename, uint8_t flags)
{
  VM_PRINTF("Open the serial port\n");

  process_start(&vm_serial_process, NULL);
  return 0;
}
/*---------------------------------------------------------------------------*/
static int
serial_read(vm_port_t *port, char *buf, size_t size)
{
#undef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
  size_t len;

  len = strlen(serial_buf) + 1;
  if(len == 0) {
    return 0;
  }

  len = MIN(len, size);

  memcpy(buf, serial_buf, len);
  serial_buf[0] = '\0';
  buf[len - 1] = '\0';
  return len;
}
/*---------------------------------------------------------------------------*/
static int
serial_write(vm_port_t *port, const char *buf, size_t size)
{
#if CONTIKI_TARGET_ZOUL
  return write(1, buf, size);
#else
  size_t i;

  for(i = 0; i < size; i++) {
    putchar(buf[i]);
  }

  return size;
#endif
}
/*---------------------------------------------------------------------------*/
const vm_port_io_t device_serial = {
  .open = serial_open,
  .read = serial_read,
  .read_object = NULL,
  .write = serial_write,
  .close = NULL
};
/*---------------------------------------------------------------------------*/
