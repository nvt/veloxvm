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

#include "vm.h"
#include "lib/sensors.h"
#include "dev/sht11/sht11-sensor.h"

/*---------------------------------------------------------------------------*/
static int
sensors_open(vm_port_t *port, const char *filename, uint8_t flags)
{
  VM_DEBUG(VM_DEBUG_MEDIUM, "Open sensor %s", filename);

  port->opaque_desc = (void *)sensors_find(filename);
  return port->opaque_desc != NULL;
}
/*---------------------------------------------------------------------------*/
static int
sensors_read(vm_port_t *port, vm_obj_t *obj)
{
  const struct sensors_sensor *sensor;

  obj->type = VM_TYPE_NONE;

  sensor = port->opaque_desc;
  if(sensor == NULL) {
    return 0;
  }

#ifdef SHT11_SENSOR_TEMP
  obj->value.integer = sensor->value(SHT11_SENSOR_TEMP);
  if(obj->value.integer != -1) {
    /* Switch object type only if we received a valid value. */
    obj->type = VM_TYPE_INTEGER;
  }
#else
  return 0;
#endif

  return 1;
}
/*---------------------------------------------------------------------------*/
static int
sensors_write(vm_port_t *port, const char *buf, size_t size)
{
  return 0;
}
/*---------------------------------------------------------------------------*/
const vm_port_io_t device_sensors = {
  .open = sensors_open,
  .read = NULL,
  .read_object = sensors_read,
  .write = sensors_write,
  .close = NULL
};
/*---------------------------------------------------------------------------*/
