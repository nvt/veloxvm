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

  if(VM_IS_CLEAR(port->flags, VM_PORT_FLAG_OPEN)) {
    vm_signal_error(thread, VM_ERROR_PORT_CLOSED);
    return NULL;
  }

  return port;
}

VM_FUNCTION(input_portp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PORT &&
                  VM_IS_SET(argv[0].value.port->flags, VM_PORT_FLAG_INPUT));
}

VM_FUNCTION(output_portp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PORT &&
                  VM_IS_SET(argv[0].value.port->flags, VM_PORT_FLAG_OUTPUT));
}

VM_FUNCTION(input_port_openp)
{
  uint8_t need = VM_PORT_FLAG_INPUT | VM_PORT_FLAG_OPEN;
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PORT &&
                  (argv[0].value.port->flags & need) == need);
}

VM_FUNCTION(output_port_openp)
{
  uint8_t need = VM_PORT_FLAG_OUTPUT | VM_PORT_FLAG_OPEN;
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PORT &&
                  (argv[0].value.port->flags & need) == need);
}

VM_FUNCTION(eof_objectp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_EOF);
}

VM_FUNCTION(eof_object)
{
  VM_PUSH_EOF();
}

VM_FUNCTION(open_input_string)
{
  vm_string_t *string;
  vm_port_t *port;

  string = argv[0].value.string;
  port = vm_string_port_open_input(thread, string->str,
                                   (size_t)string->length);
  if(port != NULL) {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(open_output_string)
{
  vm_port_t *port = vm_string_port_open_output(thread);
  if(port != NULL) {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(get_output_string)
{
  vm_string_port_get_output(thread, argv[0].value.port, &thread->result);
}

VM_FUNCTION(open_input_bytevector)
{
  vm_vector_t *vec;
  vm_port_t *port;

  vec = argv[0].value.vector;
  if(VM_IS_CLEAR(vec->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  port = vm_bytevector_port_open_input(thread, vec->bytes,
                                       (size_t)vec->length);
  if(port != NULL) {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(open_output_bytevector)
{
  vm_port_t *port = vm_bytevector_port_open_output(thread);
  if(port != NULL) {
    VM_PUSH_PORT(port);
  }
}

VM_FUNCTION(get_output_bytevector)
{
  vm_bytevector_port_get_output(thread, argv[0].value.port, &thread->result);
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

VM_FUNCTION(close_input_port)
{
  if(VM_IS_CLEAR(argv[0].value.port->flags, VM_PORT_FLAG_INPUT)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  vm_native_close_port(argv[0].value.port);
}

VM_FUNCTION(close_output_port)
{
  if(VM_IS_CLEAR(argv[0].value.port->flags, VM_PORT_FLAG_OUTPUT)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  vm_native_close_port(argv[0].value.port);
}

VM_FUNCTION(read_char)
{
  vm_port_t *port;
  vm_character_t c;
  int r;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  r = vm_native_read_char(thread, port, &c);
  if(r == VM_NATIVE_READ_OK) {
    VM_PUSH_CHARACTER(c);
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  }
  /* VM_NATIVE_READ_ERROR: vm_signal_error already issued. */
}

VM_FUNCTION(read)
{
  vm_port_t *port;
  int r;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  r = vm_native_read(thread, port, &thread->result);
  if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else {
    /* OK or ERROR: don't keep restarting; the result (or signal) is in. */
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  }
}

VM_FUNCTION(peek_char)
{
  vm_port_t *port;
  vm_character_t c;
  int r;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  r = vm_native_peek_char(thread, port, &c);
  if(r == VM_NATIVE_READ_OK) {
    VM_PUSH_CHARACTER(c);
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
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

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);

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
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else {
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  }
}

VM_FUNCTION(write)
{
  vm_port_t *port;

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);

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

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);

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

VM_FUNCTION(close_port)
{
  vm_native_close_port(argv[0].value.port);
}

VM_FUNCTION(newline)
{
  vm_port_t *port;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    return;
  }

  if(vm_native_write(port, "\n") == 0) {
    vm_signal_error(thread, VM_ERROR_IO);
  }
}

VM_FUNCTION(flush_output_port)
{
  vm_port_t *port;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    return;
  }
  vm_native_flush_port(port);
}

VM_FUNCTION(with_input_from_file)
{
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
}

VM_FUNCTION(with_output_to_file)
{
  vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
}

/* R7RS §6.13.2 binary input: (read-u8 [port]) returns the next byte
   as an exact integer in [0, 255], or an eof-object at end of stream.
   Shape mirrors read-char but pushes an integer instead of a character. */
VM_FUNCTION(read_u8)
{
  vm_port_t *port;
  vm_character_t c;
  int r;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  r = vm_native_read_char(thread, port, &c);
  if(r == VM_NATIVE_READ_OK) {
    VM_PUSH_INTEGER((vm_integer_t)((unsigned)c & 0xff));
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  }
}

/* R7RS §6.13.2 (peek-u8 [port]): same as read-u8 but does not consume
   the byte. Uses the port's one-char peek slot already populated by
   vm_native_peek_char. */
VM_FUNCTION(peek_u8)
{
  vm_port_t *port;
  vm_character_t c;
  int r;

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  r = vm_native_peek_char(thread, port, &c);
  if(r == VM_NATIVE_READ_OK) {
    VM_PUSH_INTEGER((vm_integer_t)((unsigned)c & 0xff));
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  } else if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
  }
}

/* R7RS §6.13.3 (write-u8 byte [port]): write a single byte 0-255.
   Mirrors write-char's slow-down/argument-typing structure. */
VM_FUNCTION(write_u8)
{
  vm_port_t *port;
  vm_integer_t byte;
  char buf[1];

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);

  if(argv[0].type != VM_TYPE_INTEGER ||
     (argc == 2 && argv[1].type != VM_TYPE_PORT)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  byte = argv[0].value.integer;
  if(byte < 0 || byte > 255) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }

  port = get_port(thread, argc - 1, argv + 1, VM_PORT_FLAG_OUTPUT);
  if(port == NULL) {
    return;
  }

  buf[0] = (char)(uint8_t)byte;
  if(vm_native_write_buffer(port, buf, 1) == 0) {
    vm_signal_error(thread, VM_ERROR_IO);
  }
}

/* Parse the optional [port [start [end]]] tail shared by
   read-bytevector!, write-bytevector, and write-string. argc_offset is
   the index of the first optional argument (1 for write-bytevector and
   read-bytevector!, since slot 0 holds the bytevector / string). On
   success returns the port; on type/range failure raises an error and
   returns NULL. *start_out / *end_out are populated with the resolved
   slice bounds; the caller supplies the defaults (typically 0 and the
   container length). */
static vm_port_t *
parse_io_slice_args(vm_thread_t *thread, vm_integer_t argc, vm_obj_t *argv,
                    vm_integer_t argc_offset, vm_integer_t container_len,
                    unsigned direction,
                    vm_integer_t *start_out, vm_integer_t *end_out)
{
  vm_port_t *port;
  vm_integer_t start = 0;
  vm_integer_t end = container_len;
  vm_integer_t port_argc = 0;
  vm_obj_t *port_argv = NULL;

  if(argc > argc_offset) {
    if(argv[argc_offset].type != VM_TYPE_PORT) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return NULL;
    }
    port_argc = 1;
    port_argv = argv + argc_offset;

    if(argc > argc_offset + 1) {
      if(argv[argc_offset + 1].type != VM_TYPE_INTEGER) {
        vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
        return NULL;
      }
      start = argv[argc_offset + 1].value.integer;

      if(argc > argc_offset + 2) {
        if(argv[argc_offset + 2].type != VM_TYPE_INTEGER) {
          vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
          return NULL;
        }
        end = argv[argc_offset + 2].value.integer;
      }
    }
  }

  if(start < 0 || end > container_len || start > end) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return NULL;
  }

  port = get_port(thread, port_argc, port_argv, direction);
  if(port == NULL) {
    return NULL;
  }

  *start_out = start;
  *end_out = end;
  return port;
}

/* R7RS §6.13.2 (read-bytevector k [port]): read up to k bytes into a
   fresh bytevector. Returns the bytevector (possibly shorter than k if
   EOF or a non-blocking parked port is hit mid-read), or an eof-object
   if no bytes are available before EOF. k = 0 returns an empty
   bytevector even at EOF, per R7RS. */
VM_FUNCTION(read_bytevector)
{
  vm_port_t *port;
  vm_vector_t *vec;
  vm_integer_t k;
  vm_character_t c;
  int r;
  vm_integer_t i;

  if(argv[0].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  k = argv[0].value.integer;
  if(k < 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }

  port = get_port(thread, argc - 1, argv + 1, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  if(k == 0) {
    if(vm_vector_create(&thread->result, 0, VM_VECTOR_FLAG_BUFFER) == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  /* Peek before allocating: distinguish blocked / immediate-EOF
     without leaving a wasted bytevector on the heap. */
  r = vm_native_peek_char(thread, port, &c);
  if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r != VM_NATIVE_READ_OK) {
    return;
  }

  vec = vm_vector_create(&thread->result, k, VM_VECTOR_FLAG_BUFFER);
  if(vec == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0; i < k; i++) {
    r = vm_native_read_char(thread, port, &c);
    if(r == VM_NATIVE_READ_OK) {
      vec->bytes[i] = (uint8_t)c;
    } else if(r == VM_NATIVE_READ_EOF || r == VM_NATIVE_READ_BLOCKED) {
      vec->length = i;
      VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
      return;
    } else {
      return;
    }
  }
  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
}

/* R7RS §6.13.2 (read-bytevector! bv [port [start [end]]]): read into
   the supplied bytevector at indices [start, end). Returns the
   number of bytes actually read, or an eof-object if no bytes are
   available before EOF. start = end is a no-op returning 0. */
VM_FUNCTION(read_bytevector_into)
{
  vm_port_t *port;
  vm_vector_t *vec;
  vm_integer_t start, end, count;
  vm_character_t c;
  int r;

  if(argv[0].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  vec = argv[0].value.vector;
  if(VM_IS_CLEAR(vec->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  port = parse_io_slice_args(thread, argc, argv, 1, vec->length,
                             VM_PORT_FLAG_INPUT, &start, &end);
  if(port == NULL) {
    return;
  }

  if(start == end) {
    VM_PUSH_INTEGER(0);
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  r = vm_native_peek_char(thread, port, &c);
  if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r != VM_NATIVE_READ_OK) {
    return;
  }

  for(count = 0; start + count < end; count++) {
    r = vm_native_read_char(thread, port, &c);
    if(r == VM_NATIVE_READ_OK) {
      vec->bytes[start + count] = (uint8_t)c;
    } else if(r == VM_NATIVE_READ_EOF || r == VM_NATIVE_READ_BLOCKED) {
      break;
    } else {
      return;
    }
  }
  VM_PUSH_INTEGER(count);
  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
}

/* R7RS §6.13.3 (write-bytevector bv [port [start [end]]]): write the
   slice bv[start..end] to the output port. Result is unspecified. */
VM_FUNCTION(write_bytevector)
{
  vm_port_t *port;
  vm_vector_t *vec;
  vm_integer_t start, end;

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);

  if(argv[0].type != VM_TYPE_VECTOR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  vec = argv[0].value.vector;
  if(VM_IS_CLEAR(vec->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  port = parse_io_slice_args(thread, argc, argv, 1, vec->length,
                             VM_PORT_FLAG_OUTPUT, &start, &end);
  if(port == NULL) {
    return;
  }

  if(start < end) {
    if(vm_native_write_buffer(port, (char *)(vec->bytes + start),
                              (size_t)(end - start)) == 0) {
      vm_signal_error(thread, VM_ERROR_IO);
    }
  }
}

/* R7RS §6.13.2 (read-string k [port]): textual analogue of
   read-bytevector. Returns a string of up to k characters, or an
   eof-object at EOF before any character. */
VM_FUNCTION(read_string)
{
  vm_port_t *port;
  vm_string_t *str;
  vm_integer_t k;
  vm_character_t c;
  int r;
  vm_integer_t i;

  if(argv[0].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  k = argv[0].value.integer;
  if(k < 0 || k > VM_STRING_MAX_LENGTH) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }

  port = get_port(thread, argc - 1, argv + 1, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  if(k == 0) {
    if(vm_string_create(&thread->result, 0, NULL) == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  r = vm_native_peek_char(thread, port, &c);
  if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r != VM_NATIVE_READ_OK) {
    return;
  }

  str = vm_string_create(&thread->result, k, NULL);
  if(str == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0; i < k; i++) {
    r = vm_native_read_char(thread, port, &c);
    if(r == VM_NATIVE_READ_OK) {
      str->str[i] = (char)c;
    } else if(r == VM_NATIVE_READ_EOF || r == VM_NATIVE_READ_BLOCKED) {
      str->length = i;
      str->str[i] = '\0';
      VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
      return;
    } else {
      return;
    }
  }
  str->str[k] = '\0';
  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
}

/* R7RS §6.13.3 (write-string string [port [start [end]]]). */
VM_FUNCTION(write_string)
{
  vm_port_t *port;
  vm_string_t *str;
  vm_integer_t start, end;

  if(VM_IS_SET(thread->program->flags, VM_PROGRAM_FLAG_SLOW_DOWN)) {
    vm_native_sleep(thread, VM_POLL_TIME);
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }

  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);

  if(argv[0].type != VM_TYPE_STRING) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  str = argv[0].value.string;
  if(vm_string_resolve(thread, str) == NULL) {
    return;
  }

  port = parse_io_slice_args(thread, argc, argv, 1, str->length,
                             VM_PORT_FLAG_OUTPUT, &start, &end);
  if(port == NULL) {
    return;
  }

  if(start < end) {
    if(vm_native_write_buffer(port, str->str + start,
                              (size_t)(end - start)) == 0) {
      vm_signal_error(thread, VM_ERROR_IO);
    }
  }
}

/* R7RS §6.13.2 (read-line [port]): read characters up to but not
   including the next line terminator. Recognises LF, CR, and CRLF as
   per R7RS. Returns the line as a string, or an eof-object if EOF is
   encountered before any character. Lines longer than VM_READ_LINE_MAX
   are truncated; the next read-line resumes mid-line. */
#ifndef VM_READ_LINE_MAX
#define VM_READ_LINE_MAX 1024
#endif

VM_FUNCTION(read_line)
{
  vm_port_t *port;
  vm_character_t c, next;
  int r;
  vm_integer_t i;
  char buf[VM_READ_LINE_MAX];

  port = get_port(thread, argc, argv, VM_PORT_FLAG_INPUT);
  if(port == NULL) {
    return;
  }

  r = vm_native_peek_char(thread, port, &c);
  if(r == VM_NATIVE_READ_BLOCKED) {
    VM_SET_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r == VM_NATIVE_READ_EOF) {
    VM_PUSH_EOF();
    VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
    return;
  }
  if(r != VM_NATIVE_READ_OK) {
    return;
  }

  for(i = 0; i < VM_READ_LINE_MAX; i++) {
    r = vm_native_read_char(thread, port, &c);
    if(r == VM_NATIVE_READ_OK) {
      if(c == '\n') {
        break;
      }
      if(c == '\r') {
        /* Consume a following LF to recognise CRLF as one terminator;
           a lone CR is also a terminator per R7RS. peek_char without
           consume keeps a trailing non-LF byte available to the next
           reader. */
        r = vm_native_peek_char(thread, port, &next);
        if(r == VM_NATIVE_READ_OK && next == '\n') {
          (void)vm_native_read_char(thread, port, &next);
        }
        break;
      }
      buf[i] = (char)c;
    } else if(r == VM_NATIVE_READ_EOF || r == VM_NATIVE_READ_BLOCKED) {
      break;
    } else {
      return;
    }
  }

  if(vm_string_create(&thread->result, i, NULL) == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  if(i > 0) {
    memcpy(thread->result.value.string->str, buf, (size_t)i);
  }
  thread->result.value.string->str[i] = '\0';
  VM_CLEAR_FLAG(thread->expr->flags, VM_EXPR_RESTART);
}
