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
#include "dev/radio.h"
#include "net/netstack.h"

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"

VM_DECLARE_FUNCTION(get_cca_threshold);
VM_DECLARE_FUNCTION(set_cca_threshold);
VM_DECLARE_FUNCTION(get_channel);
VM_DECLARE_FUNCTION(set_channel);
VM_DECLARE_FUNCTION(get_rssi);
VM_DECLARE_FUNCTION(get_txpower);
VM_DECLARE_FUNCTION(set_txpower);

static int load(vm_program_t *);
static int unload(vm_program_t *);

static const vm_procedure_t radio_operators[] = {
  VM_OPERATOR(get_cca_threshold, 0, 0, 0, 0),
  VM_OPERATOR(set_cca_threshold, VM_TYPE_FLAG(VM_TYPE_INTEGER),
              VM_PROCEDURE_EVAL_ARGS, 1, 1),
  VM_OPERATOR(get_channel, 0, 0, 0, 0),
  VM_OPERATOR(set_channel, VM_TYPE_FLAG(VM_TYPE_INTEGER),
              VM_PROCEDURE_EVAL_ARGS, 1, 1),
  VM_OPERATOR(get_rssi, 0, 0, 0, 0),
  VM_OPERATOR(get_txpower, 0, 0, 0, 0),
  VM_OPERATOR(set_txpower, VM_TYPE_FLAG(VM_TYPE_INTEGER),
              VM_PROCEDURE_EVAL_ARGS, 1, 1)
};

vm_lib_t vm_lib_radio = {
  .name = "radio",
  .load = load,
  .unload = unload,
  .operators = radio_operators,
  .operator_count = ARRAY_SIZE(radio_operators),
  .symbols = (const char *[]){"get-cca-threshold", "set-cca-threshold",
                              "get-channel", "set-channel",
                              "get-rssi", "get-txpower", "set-txpower"},
  .symbol_count = 7
};

static int
load(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_MEDIUM, "Loading the radio library");

  return 1;
}

static int
unload(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_MEDIUM, "Unloading the radio library");
  return 1;
}

static void
handle_radio_result(vm_thread_t *thread,
                    const char *funcname,
                    radio_result_t r)
{
  const char *error_string;

  switch(r) {
  case RADIO_RESULT_OK:
    error_string = "OK";
    break;
  case RADIO_RESULT_NOT_SUPPORTED:
    error_string = "param not supported";
    break;
  case RADIO_RESULT_INVALID_VALUE:
    error_string = "invalid value";
    break;
  case RADIO_RESULT_ERROR:
    error_string = "radio error";
    break;
  default:
    error_string = "unknown result";
    break;
  }

  VM_DEBUG(VM_DEBUG_LOW, "lib-radio %s: %s",
           funcname, error_string);
}

static void
get_param(vm_thread_t *thread, radio_param_t param)
{
  radio_result_t r;
  radio_value_t v;

  r = NETSTACK_RADIO.get_value(param, &v);
  handle_radio_result(thread, __func__, r);
  if(r == RADIO_RESULT_OK) {
    VM_PUSH_INTEGER(v);
  }
}

static void set_param(vm_thread_t *thread, radio_param_t param,
                      radio_value_t value)
{
  radio_result_t r;

  r = NETSTACK_RADIO.set_value(param, value);
  handle_radio_result(thread, __func__, r);
  VM_PUSH_BOOLEAN(r == RADIO_RESULT_OK);
}

VM_FUNCTION(get_cca_threshold)
{
  get_param(thread, RADIO_PARAM_CCA_THRESHOLD);
}

VM_FUNCTION(set_cca_threshold)
{
  set_param(thread, RADIO_PARAM_CCA_THRESHOLD, argv[0].value.integer);
}

VM_FUNCTION(get_channel)
{
  get_param(thread, RADIO_PARAM_CHANNEL);
}

VM_FUNCTION(set_channel)
{
  set_param(thread, RADIO_PARAM_CHANNEL, argv[0].value.integer);
}

VM_FUNCTION(get_rssi)
{
  get_param(thread, RADIO_PARAM_RSSI);
}

VM_FUNCTION(get_txpower)
{
  get_param(thread, RADIO_PARAM_TXPOWER);
}

VM_FUNCTION(set_txpower)
{
  set_param(thread, RADIO_PARAM_TXPOWER, argv[0].value.integer);
}
