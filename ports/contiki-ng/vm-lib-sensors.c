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

#include "contiki.h"

#include "vm.h"
#include "vm-lib.h"
#include "vm-list.h"
#include "vm-log.h"

#if !CONTIKI_TARGET_NATIVE
#include "lib/sensors.h"
#endif

VM_DECLARE_FUNCTION(get_sensors);
VM_DECLARE_FUNCTION(sensor_value);

static int load(vm_program_t *);
static int unload(vm_program_t *);

/* Two entries per operator: the canonical hyphenated symbol for
 * Scheme callers and an underscore alias for Python callers (hyphens
 * are not valid in Python identifiers).  Both aliases resolve to the
 * same VM_FUNCTION via vm_lib_load's symbols[i] -> operators[i]
 * mapping. */
static const vm_procedure_t sensor_operators[] = {
  VM_OPERATOR(get_sensors, VM_TYPE_FLAG_ANY, VM_PROCEDURE_EVAL_ARGS, 0, 0),
  VM_OPERATOR(sensor_value, VM_TYPE_FLAG(VM_TYPE_STRING),
              VM_PROCEDURE_EVAL_ARGS, 1, 1),
  VM_OPERATOR(get_sensors, VM_TYPE_FLAG_ANY, VM_PROCEDURE_EVAL_ARGS, 0, 0),
  VM_OPERATOR(sensor_value, VM_TYPE_FLAG(VM_TYPE_STRING),
              VM_PROCEDURE_EVAL_ARGS, 1, 1)
};

vm_lib_t vm_lib_sensors = {
  .name = "sensors",
  .load = load,
  .unload = unload,
  .operators = sensor_operators,
  .operator_count = VM_ARRAY_SIZE(sensor_operators),
  .symbols = (const char *[]){"get-sensors", "sensor-value",
                              "get_sensors", "sensor_value"},
  .symbol_count = 4
};

static int
load(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_LOW, "Loading the sensors library");
  return 1;
}

static int
unload(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_LOW, "Unloading the sensors library");
  return 1;
}

#if CONTIKI_TARGET_NATIVE
/* The Contiki-NG native target omits lib/sensors.c from its build,
 * so there are no sensors to enumerate. Return empty / false to keep
 * user programs runnable on the host. */

VM_FUNCTION(get_sensors)
{
  vm_list_t *list;

  list = vm_list_create();
  if(list == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  thread->result.type = VM_TYPE_LIST;
  thread->result.value.list = list;
}

VM_FUNCTION(sensor_value)
{
  VM_PUSH_BOOLEAN(VM_FALSE);
}

#else /* !CONTIKI_TARGET_NATIVE */

VM_FUNCTION(get_sensors)
{
  const struct sensors_sensor *sensor;
  vm_list_t *list;
  vm_obj_t elem;

  vm_gc_disable();

  list = vm_list_create();
  if(list == NULL) {
    vm_gc_enable();
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(sensor = sensors_first(); sensor != NULL; sensor = sensors_next(sensor)) {
    /* Skip entries without a read function (pure actuators such as
     * relays).  Callers then know every listed name is safe to pass
     * to sensor-value. */
    if(sensor->value == NULL) {
      continue;
    }
    if(vm_string_create(&elem, -1, sensor->type) == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    if(!vm_list_insert_tail(list, &elem)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  vm_gc_enable();
  thread->result.type = VM_TYPE_LIST;
  thread->result.value.list = list;
}

VM_FUNCTION(sensor_value)
{
  const struct sensors_sensor *sensor;

  sensor = sensors_find(argv[0].value.string->str);
  if(sensor == NULL || sensor->value == NULL) {
    /* Unknown sensor or actuator-only entry.  Signal a proper error
     * rather than returning #f: all names from get-sensors are
     * guaranteed to be readable, so this path is a programming error
     * in the caller. */
    vm_signal_error(thread, VM_ERROR_NAME);
    vm_set_error_string(thread, "no such sensor");
    return;
  }

  /* Activate on demand: many drivers return -1 on the first read
   * otherwise, and leaving every sensor permanently active would
   * waste power.  Callers that need a steady stream can do their
   * own configure(). */
  if(sensor->configure != NULL) {
    sensor->configure(SENSORS_ACTIVE, 1);
  }

  VM_PUSH_INTEGER(sensor->value(0));
}

#endif /* CONTIKI_TARGET_NATIVE */
