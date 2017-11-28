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

#include "vm.h"
#include "vm-functions.h"
#include "vm-log.h"

VM_FUNCTION(construct_packet)
{
  vm_vector_t *fields;
  vm_vector_t *values;
  vm_vector_t *packet;
  vm_integer_t packet_length;
  int i;
  int j;
  size_t position;
  uint8_t field_bits;
  uint64_t value;
  uint8_t bits_in_current_byte;
  vm_vector_t *value_vector;

  fields = argv[0].value.vector;
  values = argv[1].value.vector;

  if(fields->length != values->length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_string(thread, "vector lengths must match");
    return;
  }

  packet_length = 0;
  for(i = 0; i < fields->length; i++) {
    if(fields->elements[i].type != VM_TYPE_INTEGER ||
       fields->elements[i].value.integer <= 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      vm_set_error_string(thread, "field elements must be positive integers");
      return;
    }
    packet_length += fields->elements[i].value.integer;
  }

  VM_DEBUG(VM_DEBUG_MEDIUM, "Packet length: %u bits\n",
           (unsigned)packet_length);

  /* Require the specified format to be a multiple of 8 bits. */
  if(packet_length & 0x7) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_string(thread, "packet lengths must be multiples of 8 bits");
    return;
  }

  packet_length /= 8;

  packet = vm_vector_create(&thread->result, packet_length,
                            VM_VECTOR_FLAG_BUFFER);
  if(packet == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = position = 0; i < fields->length; i++) {
    field_bits = fields->elements[i].value.integer;

    if(values->elements[i].type == VM_TYPE_VECTOR) {
      value_vector = values->elements[i].value.vector;

      /*
       * If the value is a vector, we must write this on a byte-aligned
       * position within the packet. Furthermore, the number of elements in
       * the vector must match the number of bytes of the current field.
       */
      if(field_bits & 0x7 || position & 0x7 ||
         value_vector->length != field_bits / 8) {
        vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
        vm_set_error_string(thread, "field and value lengths do not match");
        return;
      }

      for(j = 0; j < value_vector->length; j++) {
        if(value_vector->elements[j].type != VM_TYPE_INTEGER ||
           value_vector->elements[j].value.integer > 255) {
          vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
          vm_set_error_string(thread, "integers in the value vector must 0 <= x <= 255");
          return;
        }

        VM_DEBUG(VM_DEBUG_MEDIUM,
                  "Write vector: value 0x%x, %u bits at byte %u, bit %u\n",
                  value_vector->elements[j].value.integer,
                  8, position / 8, position & 0x7);

        packet->bytes[position / 8] = value_vector->elements[j].value.integer;
        position += 8;
      }
    } else {
      value = values->elements[i].value.integer;

      VM_DEBUG(VM_DEBUG_MEDIUM,
               "Write number: %u bits with value %u at position %u\n",
               (unsigned)field_bits, (unsigned)value, (unsigned)position);

      if(field_bits > 8) {
        /* We can only write from even byte boundaries when field_bits is
           over 8. */
        if(position & 0x7) {
          vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
          vm_set_error_string(thread, "multi-byte write starting inside byte");
          return;
        }

        if(field_bits > 64) {
          vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
          vm_set_error_string(thread, "field bits exceed 64 bits");
          return;
        }

        if(field_bits & (field_bits - 1)) {
          vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
          vm_set_error_string(thread,
            "field bits must be a multiple of 8 when exceeding 8");
          return;
        }

        while(field_bits > 7) {
          packet->bytes[position / 8] = (value >> (field_bits - 8)) & 0xff;
          position += 8;
          field_bits -= 8;
        }

        continue;
      }

      while(field_bits > 0) {
        bits_in_current_byte = VM_MIN(8 - (position & 0x7), field_bits);

        VM_DEBUG(VM_DEBUG_MEDIUM,
                  "Write value %u, %u bits at byte %u, bit %u\n",
                  value & ((1 << bits_in_current_byte) - 1),
                  bits_in_current_byte,
                  position / 8, position & 0x7);

        packet->bytes[position / 8] |= (value & ((1 << bits_in_current_byte) - 1)) <<
                                (position & 0x7);
        position += bits_in_current_byte;
        field_bits -= bits_in_current_byte;
      }
    }
  }
}

VM_FUNCTION(deconstruct_packet)
{
  vm_vector_t *fields;
  vm_vector_t *values;
  vm_vector_t *packet;
  vm_integer_t packet_length;
  int i;
  size_t position;
  vm_integer_t field_length;

  fields = argv[0].value.vector;
  packet = argv[1].value.vector;

  if(IS_CLEAR(packet->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  packet_length = 0;
  for(i = 0; i < fields->length; i++) {
    if(fields->elements[i].type != VM_TYPE_INTEGER ||
       fields->elements[i].value.integer <= 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      vm_set_error_string(thread, "field elements must be positive integers");
      return;
    }
    packet_length += fields->elements[i].value.integer;
  }

  if(packet->length * 8 != packet_length) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_string(thread, "packet length does not match the sum of field length");
    return;
  }

  VM_DEBUG(VM_DEBUG_MEDIUM, "Packet length: %u bits\n",
           (unsigned)packet_length);

  /* Require the specified format to be a multiple of 8 bits. */
  if(packet_length & 0x7) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    vm_set_error_string(thread, "packet lengths must be multiples of 8 bits");
    return;
  }

  packet_length /= 8;

  values = vm_vector_create(&thread->result, fields->length,
                            VM_VECTOR_FLAG_REGULAR);
  if(values == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = position = 0; i < fields->length; i++) {
    field_length = fields->elements[i].value.integer;

    values->elements[i].type = VM_TYPE_INTEGER;

    values->elements[i].value.integer = 0;
    while(field_length > 8) {
      values->elements[i].value.integer |= packet->bytes[position / 8];
      values->elements[i].value.integer <<= 8;
      field_length -= 8;
      position += 8;
    }

    if(field_length > 0) {
      values->elements[i].value.integer |= packet->bytes[position / 8] & ((1 << field_length) - 1);
      position += field_length;
    }

    VM_PRINTF("Integer %d. field length %d\n",
              (int)values->elements[i].value.integer, (int)field_length);
  }
}
