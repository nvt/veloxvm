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

static vm_port_t *
get_port(vm_thread_t *thread, vm_integer_t argc,
	 vm_obj_t *argv, unsigned direction)
{
  vm_port_t *port;

  if(argc == 0) {
    if(direction == VM_PORT_FLAG_INPUT &&
       !vm_policy_check_resources(thread, VM_POLICY_RESOURCE_CONSOLE)) {
      return NULL;
    }

    port = vm_native_default_port(thread, direction);
    if(port == NULL) {
      vm_signal_error(thread, VM_ERROR_IO);
      return NULL;
    }
  } else {
    if(argv[0].type != VM_TYPE_PORT) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return 0;
    }
    port = argv[0].value.port;
  }

  if(IS_CLEAR(port->flags, VM_PORT_FLAG_OPEN)) {
    vm_signal_error(thread, VM_ERROR_PORT_CLOSED);
    return NULL;
  }

  return port;
}

VM_FUNCTION(input_portp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PORT &&
                  IS_SET(argv[0].value.port->flags, VM_PORT_FLAG_INPUT));
}

VM_FUNCTION(output_portp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PORT &&
                  IS_SET(argv[0].value.port->flags, VM_PORT_FLAG_OUTPUT));
}

VM_FUNCTION(eof_objectp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_CHARACTER &&
                  argv[0].value.character == '\0');
}

VM_FUNCTION(current_input_port)
{
  vm_port_t *port;

  port = vm_native_default_port(thread, VM_PORT_FLAG_INPUT);
  if(port != NULL) {
    VM_PUSH_PORT(port);
  } else {
    vm_signal_error(thread, VM_ERROR_IO);
  }
}

VM_FUNCTION(current_output_port)
{
  vm_port_t *port;

  port = vm_native_default_port(thread, VM_PORT_FLAG_OUTPUT);
  if(port != NULL) {
    VM_PUSH_PORT(port);
  } else {
    vm_signal_error(thread, VM_ERROR_IO);
  }
}

VM_FUNCTION(open_input_file)
{
  vm_port_t *port;
  const char *filename;

  filename = argv[0].value.string->str;

  vm_policy_check_file(thread, filename, VM_PORT_FLAG_INPUT);

  port = vm_native_open_file(thread, filename, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, filename);
  } else {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(open_output_file)
{
  vm_port_t *port;
  const char *filename;

  filename = argv[0].value.string->str;

  vm_policy_check_file(thread, filename, VM_PORT_FLAG_OUTPUT);

  port = vm_native_open_file(thread, filename, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    vm_signal_error(thread, VM_ERROR_IO);
    vm_set_error_string(thread, filename);
  } else {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(close_port)
{
  vm_native_close_port(argv[0].value.port);
}

VM_FUNCTION(read_char)
{
  vm_port_t *port;
  vm_character_t c;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  /* Temporarily assign the default input port to this thread. */
  port->thread = thread;

  if(vm_native_read_char(port, &c) == 1) {
    VM_PUSH_CHARACTER(c);
    CLEAR(thread->expr->flags, VM_EXPR_RESTART);
  } else if(thread->status == VM_THREAD_WAITING) {
    SET(thread->expr->flags, VM_EXPR_RESTART);
  }
}

VM_FUNCTION(read)
{
  vm_port_t *port;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  /* Temporarily assign the default input port to this thread. */
  port->thread = thread;

  if(vm_native_read(port, &thread->result) == 0) {
    VM_PUSH_CHARACTER('\0');
  }

  if(thread->status == VM_THREAD_WAITING) {
    SET(thread->expr->flags, VM_EXPR_RESTART);
  } else {
    CLEAR(thread->expr->flags, VM_EXPR_RESTART);
  }
}

VM_FUNCTION(peek_char)
{
  vm_character_t c;

  if(vm_native_peek_char(argc == 1 ? argv[0].value.port : NULL, &c) == 1) {
    VM_PUSH_CHARACTER(c);
  } else {
    VM_PUSH_CHARACTER('\0');
  }
}

VM_FUNCTION(char_readyp)
{
  vm_port_t *port;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  VM_PUSH_BOOLEAN(vm_native_char_readyp(port));
}

VM_FUNCTION(write_char)
{
  vm_port_t *port;

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    SET(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  CLEAR(thread->expr->flags, VM_EXPR_RESTART);

  if(argv[0].type != VM_TYPE_CHARACTER ||
     (argc == 2 && argv[1].type != VM_TYPE_PORT)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  port = get_port(thread, argc - 1, argv + 1, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    return;
  }

  if(vm_native_write(port, "%c", argv[0].value.character) == 0) {
    vm_signal_error(thread, VM_ERROR_IO);
    return;
  }

  if(thread->status == VM_THREAD_WAITING) {
    SET(thread->expr->flags, VM_EXPR_RESTART);
  } else {
    CLEAR(thread->expr->flags, VM_EXPR_RESTART);
  }
}

VM_FUNCTION(write)
{
  vm_port_t *port;

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    SET(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  CLEAR(thread->expr->flags, VM_EXPR_RESTART);

  if(argc == 2 && argv[1].type != VM_TYPE_PORT) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  port = get_port(thread, argc - 1, argv + 1, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    return;
  }

  vm_write_object(port, &argv[0]);
}

VM_FUNCTION(display)
{
  vm_port_t *port;

  if(IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    SET(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  CLEAR(thread->expr->flags, VM_EXPR_RESTART);

  if(argc == 2 && argv[1].type != VM_TYPE_PORT) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  port = get_port(thread, argc - 1, argv + 1, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    return;
  }

  vm_write_object(port, &argv[0]);
}

VM_FUNCTION(with_input_from_file)
{
}

VM_FUNCTION(with_output_to_file)
{
}
