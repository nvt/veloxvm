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
#include "dev/leds.h"

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"

struct sym_led_pair {
  const char *sym_name;
  unsigned char led;
};

static const struct sym_led_pair sym_to_led_map[] = {
  {"LEDRed",   LEDS_RED},
  {"LEDGreen", LEDS_GREEN},
  {"LEDBlue",  LEDS_BLUE},
  {"LEDAll",   LEDS_ALL}
};

VM_DECLARE_FUNCTION(leds_on);
VM_DECLARE_FUNCTION(leds_off);
VM_DECLARE_FUNCTION(leds_set);
VM_DECLARE_FUNCTION(leds_get);
VM_DECLARE_FUNCTION(leds_toggle);

static int load(vm_program_t *);
static int unload(vm_program_t *);

static const vm_procedure_t leds_operators[] = {
  VM_OPERATOR(leds_on, VM_TYPE_FLAG(VM_TYPE_SYMBOL),
              VM_PROCEDURE_EVAL_ARGS, 1, -1),
  VM_OPERATOR(leds_off, VM_TYPE_FLAG(VM_TYPE_SYMBOL),
              VM_PROCEDURE_EVAL_ARGS, 1, -1),
  VM_OPERATOR(leds_set, VM_TYPE_FLAG(VM_TYPE_SYMBOL),
              VM_PROCEDURE_EVAL_ARGS, 1, -1),
  VM_OPERATOR(leds_get, 0, 0, 0, 0),
  VM_OPERATOR(leds_toggle, VM_TYPE_FLAG(VM_TYPE_SYMBOL),
              VM_PROCEDURE_EVAL_ARGS, 1, -1)
};

vm_lib_t vm_lib_leds = {
  .name = "leds",
  .load = load,
  .unload = unload,
  .operators = leds_operators,
  .operator_count = ARRAY_SIZE(leds_operators),
  .symbols = (const char *[]){"leds-on", "leds-off", "leds-set",
              "leds-get", "leds-toggle"},
  .symbol_count = 5
};

static int
load(vm_program_t *program)
{
  vm_obj_t obj;

  VM_DEBUG(VM_DEBUG_LOW, "Loading the LEDs library");

  /* Bind all library symbols to a dummy value. */
  obj.type = VM_TYPE_BOOLEAN;
  obj.value.boolean = VM_TRUE;

  vm_lib_bind_symbol(program, "LEDAll", &obj);
  vm_lib_bind_symbol(program, "LEDRed", &obj);
  vm_lib_bind_symbol(program, "LEDGreen", &obj);
  vm_lib_bind_symbol(program, "LEDBlue", &obj);

  return 1;
}

static int
unload(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_LOW, "Unloading the LEDs library");
  return 1;
}

static unsigned char
get_led_value(vm_program_t *program, vm_symbol_ref_t *symref)
{
  const char *name;
  int i;

  name = vm_symbol_lookup(program, symref);
  if(name == NULL) {
    return 0;
  }

  for(i = 0; i < ARRAY_SIZE(sym_to_led_map); i++) {
    if(strcasecmp(name, sym_to_led_map[i].sym_name) == 0) {
      return sym_to_led_map[i].led;
    }
  }

  return 0;
}

VM_FUNCTION(leds_on)
{
  leds_on(get_led_value(thread->program, &argv[0].value.symbol_ref));
}

VM_FUNCTION(leds_off)
{
  leds_off(get_led_value(thread->program, &argv[0].value.symbol_ref));
}

VM_FUNCTION(leds_set)
{
  leds_set(get_led_value(thread->program, &argv[0].value.symbol_ref));
}

VM_FUNCTION(leds_get)
{
  leds_get();
}

VM_FUNCTION(leds_toggle)
{
  if(VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM) {
    VM_PRINTF("Toggle LEDs ");
    vm_write_object(NULL, &argv[0]);
    VM_PRINTF("\n");
  }

  leds_toggle(get_led_value(thread->program, &argv[0].value.symbol_ref));
}
