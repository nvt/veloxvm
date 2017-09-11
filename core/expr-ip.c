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

#include "vm-functions.h"
#include "vm-log.h"
#include "vm-native.h"

struct protocol {
  const char *name;
  vm_socket_type_t socket_type;
};

const struct protocol protocols[] = {
  { .name = "TCP", .socket_type = VM_SOCKET_STREAM},
  { .name = "UDP", .socket_type = VM_SOCKET_DATAGRAM},
};

static const struct protocol *
find_protocol(const char *protocol)
{
  size_t i;

  if(protocol == NULL) {
    return NULL;
  }

  for(i = 0; i < ARRAY_SIZE(protocols); i++) {
    if(strcmp(protocol, protocols[i].name) == 0) {
      return &protocols[i];
    }
  }

  return NULL;
}

VM_FUNCTION(make_client)
{
  const char *protocol_name;
  const struct protocol *protocol;
  vm_port_t *port;
  int status;
  vm_vector_t *addr;
  vm_integer_t dst_port;
  vm_integer_t local_port;

  if(argv[0].type != VM_TYPE_SYMBOL ||
     (argv[1].type != VM_TYPE_VECTOR && argv[1].type != VM_TYPE_STRING) ||
     argv[2].type != VM_TYPE_INTEGER ||
     (argc == 4 && argv[3].type != VM_TYPE_INTEGER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  protocol_name = vm_symbol_lookup(thread->program,
                                   &argv[0].value.symbol_ref);
  protocol = find_protocol(protocol_name);
  if(protocol == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[0]);
    return;
  }

  if(argv[1].type == VM_TYPE_STRING) {
    status = vm_native_resolve(thread, argv[1].value.string->str);
    if(status > 0) {
      addr = thread->result.value.vector;
    } else if(status == 0) {
      return;
    } else if(status < 0) {
      vm_signal_error(thread, VM_ERROR_SOCKET);
      vm_set_error_string(thread, "unable to resolve hostname");
      return;
    }
  } else {
    addr = argv[1].value.vector;
  }

  dst_port = argv[2].value.integer;

  if(!vm_policy_check_net(thread, addr, dst_port)) {
    return;
  }

  local_port = argc == 4 ? argv[3].value.integer : 0;

  port = vm_native_open_client(thread, protocol->socket_type,
			       addr, dst_port, local_port);
  if(port == NULL) {
    vm_signal_error(thread, VM_ERROR_SOCKET);
  } else {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(make_server)
{
  const char *protocol_name;
  vm_port_t *port;

  if(argv[0].type != VM_TYPE_SYMBOL ||
     argv[1].type != VM_TYPE_VECTOR ||
     argv[2].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  protocol_name = vm_symbol_lookup(thread->program,
                                   &argv[0].value.symbol_ref);
  if(protocol_name == NULL || find_protocol(protocol_name) == 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_object(thread, &argv[0]);
    return;
  }

  port = vm_native_open_server(thread,
			       argv[1].value.vector,
			       argv[2].value.integer);
  if(port == NULL) {
    vm_signal_error(thread, VM_ERROR_SOCKET);
  } else {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(peer_name)
{
  vm_port_t *port;

  port = argv[0].value.port;

  if(IS_SET(port->flags, VM_PORT_FLAG_SOCKET)) {
    vm_native_get_peer_name(thread, port, &thread->result);
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  }
}

VM_FUNCTION(accept_client)
{
  vm_native_accept_client(thread, argv[0].value.port, &thread->result);
}

VM_FUNCTION(incoming_clientp)
{
  vm_native_incoming_clientp(thread, argv[0].value.port, &thread->result);
}

VM_FUNCTION(addr_to_string)
{
  vm_vector_t *vector;
  vm_string_t *string;
  int i, j;
  uint8_t byte;

  vector = argv[0].value.vector;

  switch(vector->length) {
  case 4:
    string = vm_string_create(&thread->result, vector->length * 4 - 1, NULL);
    if(string == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    for(i = j = 0; i < 4; i++) {
      if(vector->elements[i].type != VM_TYPE_INTEGER) {
        vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
        return;
      }

      byte = vector->elements[i].value.integer & 0xff;
      if(byte > 100) {
        string->str[j++] = '0' + byte / 100;
        byte -= 100 * (byte / 100);
      }
      if(byte > 10) {
        string->str[j++] = '0' + byte / 10;
        byte -= 10 * (byte / 10);
      }
      string->str[j++] = '0' + byte;

      if(i < 3) {
        string->str[j++] = '.';
      }
    }

    string->str[j] = '\0';
    break;
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  }
}

VM_FUNCTION(resolve_hostname)
{
  int status;
  vm_string_t *string;

  string = argv[0].value.string;

  VM_DEBUG(VM_DEBUG_MEDIUM, "Attempting to resolve \"%s\"...", string->str);
  status = vm_native_resolve(thread, string->str);
  /* If status is negative, the resolving failed an we return a false value.
     All other status values are handled automatically. */
  if(status < 0) {
    VM_PUSH_BOOLEAN(VM_FALSE);
  }
}
