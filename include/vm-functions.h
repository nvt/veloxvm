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

#ifndef VM_FUNCTIONS_H
#define VM_FUNCTIONS_H

#include "vm.h"

#define VM_OPERATOR(name, types, flags, min_argc, max_argc) \
  {op_##name, 0, (types), (min_argc), (max_argc), (flags)}

#define VM_DECLARE_FUNCTION(name)					\
  extern void op_##name(vm_thread_t *, vm_type_set_t, vm_integer_t, vm_obj_t *)
#define VM_FUNCTION(name)					\
  void op_##name(vm_thread_t * thread, vm_type_set_t type_set, \
                 vm_integer_t argc, vm_obj_t *argv)

/* I/O functions. */
VM_DECLARE_FUNCTION(input_portp);
VM_DECLARE_FUNCTION(output_portp);
VM_DECLARE_FUNCTION(current_input_port);
VM_DECLARE_FUNCTION(current_output_port);
VM_DECLARE_FUNCTION(open_input_file);
VM_DECLARE_FUNCTION(open_output_file);
VM_DECLARE_FUNCTION(close_port);
VM_DECLARE_FUNCTION(read_char);
VM_DECLARE_FUNCTION(read);
VM_DECLARE_FUNCTION(peek_char);
VM_DECLARE_FUNCTION(eof_objectp);
VM_DECLARE_FUNCTION(char_readyp);
VM_DECLARE_FUNCTION(write_char);
VM_DECLARE_FUNCTION(write);
VM_DECLARE_FUNCTION(display);
VM_DECLARE_FUNCTION(with_input_from_file);
VM_DECLARE_FUNCTION(with_output_to_file);

/* Mathematical functions. */
VM_DECLARE_FUNCTION(add);
VM_DECLARE_FUNCTION(subtract);
VM_DECLARE_FUNCTION(multiply);
VM_DECLARE_FUNCTION(divide);
VM_DECLARE_FUNCTION(gcd);
VM_DECLARE_FUNCTION(lcm);
VM_DECLARE_FUNCTION(numerator);
VM_DECLARE_FUNCTION(denominator);
VM_DECLARE_FUNCTION(quotient);
VM_DECLARE_FUNCTION(remainder);
VM_DECLARE_FUNCTION(modulo);

/* Comparison functions. */
VM_DECLARE_FUNCTION(equal);
VM_DECLARE_FUNCTION(different);
VM_DECLARE_FUNCTION(less_than);
VM_DECLARE_FUNCTION(less_than_equal);
VM_DECLARE_FUNCTION(greater_than);
VM_DECLARE_FUNCTION(greater_than_equal);
VM_DECLARE_FUNCTION(zerop);

/* Primitive functions. */
VM_DECLARE_FUNCTION(bind);
VM_DECLARE_FUNCTION(return);
VM_DECLARE_FUNCTION(begin);
VM_DECLARE_FUNCTION(if);
VM_DECLARE_FUNCTION(define);
VM_DECLARE_FUNCTION(set);
VM_DECLARE_FUNCTION(and);
VM_DECLARE_FUNCTION(or);
VM_DECLARE_FUNCTION(apply);
VM_DECLARE_FUNCTION(quote);
VM_DECLARE_FUNCTION(call_with_cc);
VM_DECLARE_FUNCTION(values);
VM_DECLARE_FUNCTION(call_with_values);
VM_DECLARE_FUNCTION(dynamic_wind);
VM_DECLARE_FUNCTION(eval);

VM_DECLARE_FUNCTION(numberp);
VM_DECLARE_FUNCTION(integerp);
VM_DECLARE_FUNCTION(rationalp);
VM_DECLARE_FUNCTION(realp);
VM_DECLARE_FUNCTION(complexp);
VM_DECLARE_FUNCTION(exactp);
VM_DECLARE_FUNCTION(inexactp);
VM_DECLARE_FUNCTION(procedurep);
VM_DECLARE_FUNCTION(booleanp);
VM_DECLARE_FUNCTION(portp);
VM_DECLARE_FUNCTION(not);
VM_DECLARE_FUNCTION(eqp);
VM_DECLARE_FUNCTION(eqvp);
VM_DECLARE_FUNCTION(equalp);

/* List functions. */
VM_DECLARE_FUNCTION(list);
VM_DECLARE_FUNCTION(cons);
VM_DECLARE_FUNCTION(push);
VM_DECLARE_FUNCTION(pop);
VM_DECLARE_FUNCTION(car);
VM_DECLARE_FUNCTION(cdr);
VM_DECLARE_FUNCTION(list_ref);
VM_DECLARE_FUNCTION(list_tail);
VM_DECLARE_FUNCTION(append);
VM_DECLARE_FUNCTION(remove);
VM_DECLARE_FUNCTION(reverse);
VM_DECLARE_FUNCTION(length);
VM_DECLARE_FUNCTION(nullp);
VM_DECLARE_FUNCTION(listp);
VM_DECLARE_FUNCTION(pairp);
VM_DECLARE_FUNCTION(set_car);
VM_DECLARE_FUNCTION(set_cdr);
VM_DECLARE_FUNCTION(memq);
VM_DECLARE_FUNCTION(memv);
VM_DECLARE_FUNCTION(member);
VM_DECLARE_FUNCTION(assq);
VM_DECLARE_FUNCTION(assv);
VM_DECLARE_FUNCTION(assoc);

/* Higher-order list functions. */
VM_DECLARE_FUNCTION(map);
VM_DECLARE_FUNCTION(filter);
VM_DECLARE_FUNCTION(for_each);
VM_DECLARE_FUNCTION(reduce);
VM_DECLARE_FUNCTION(count);

/* Character functions. */
VM_DECLARE_FUNCTION(charp);
VM_DECLARE_FUNCTION(char_compare);
VM_DECLARE_FUNCTION(char_class);
VM_DECLARE_FUNCTION(char_to_integer);
VM_DECLARE_FUNCTION(integer_to_char);
VM_DECLARE_FUNCTION(char_upcase);
VM_DECLARE_FUNCTION(char_downcase);

/* String functions. */
VM_DECLARE_FUNCTION(make_string);
VM_DECLARE_FUNCTION(string);
VM_DECLARE_FUNCTION(stringp);
VM_DECLARE_FUNCTION(string_length);
VM_DECLARE_FUNCTION(string_ref);
VM_DECLARE_FUNCTION(string_set);
VM_DECLARE_FUNCTION(string_to_list);
VM_DECLARE_FUNCTION(list_to_string);
VM_DECLARE_FUNCTION(vector_to_string);
VM_DECLARE_FUNCTION(string_fill);
VM_DECLARE_FUNCTION(string_compare);
VM_DECLARE_FUNCTION(substring);
VM_DECLARE_FUNCTION(string_append);
VM_DECLARE_FUNCTION(string_to_list);
VM_DECLARE_FUNCTION(list_to_string);
VM_DECLARE_FUNCTION(string_copy);
VM_DECLARE_FUNCTION(string_split);
VM_DECLARE_FUNCTION(number_to_string);
VM_DECLARE_FUNCTION(string_to_number);

/* System functions. */
VM_DECLARE_FUNCTION(system_info);
VM_DECLARE_FUNCTION(load_program);
VM_DECLARE_FUNCTION(import);
VM_DECLARE_FUNCTION(get_devices);
VM_DECLARE_FUNCTION(print);
VM_DECLARE_FUNCTION(random);
VM_DECLARE_FUNCTION(time);
VM_DECLARE_FUNCTION(get_programs);
VM_DECLARE_FUNCTION(program_info);
VM_DECLARE_FUNCTION(exit);

/* Exception and condition functions. */
VM_DECLARE_FUNCTION(guard);
VM_DECLARE_FUNCTION(raise);

/* Thread functions. */
VM_DECLARE_FUNCTION(thread_create);
VM_DECLARE_FUNCTION(thread_fork);
VM_DECLARE_FUNCTION(thread_id);
VM_DECLARE_FUNCTION(thread_join);
VM_DECLARE_FUNCTION(thread_sleep);
VM_DECLARE_FUNCTION(thread_specific);
VM_DECLARE_FUNCTION(thread_specific_set);
VM_DECLARE_FUNCTION(thread_terminate);
VM_DECLARE_FUNCTION(thread_yield);
VM_DECLARE_FUNCTION(thread_stats);

/* Mutex functions. */
VM_DECLARE_FUNCTION(mutexp);
VM_DECLARE_FUNCTION(make_mutex);
VM_DECLARE_FUNCTION(mutex_name);
VM_DECLARE_FUNCTION(mutex_specific);
VM_DECLARE_FUNCTION(mutex_specific_set);
VM_DECLARE_FUNCTION(mutex_state);
VM_DECLARE_FUNCTION(mutex_lock);
VM_DECLARE_FUNCTION(mutex_unlock);

/* Vector functions. */
VM_DECLARE_FUNCTION(make_vector);
VM_DECLARE_FUNCTION(vector);
VM_DECLARE_FUNCTION(vectorp);
VM_DECLARE_FUNCTION(bufferp);
VM_DECLARE_FUNCTION(vector_merge);
VM_DECLARE_FUNCTION(vector_length);
VM_DECLARE_FUNCTION(vector_ref);
VM_DECLARE_FUNCTION(vector_set);
VM_DECLARE_FUNCTION(vector_to_list);
VM_DECLARE_FUNCTION(list_to_vector);
VM_DECLARE_FUNCTION(vector_fill);
VM_DECLARE_FUNCTION(make_buffer);
VM_DECLARE_FUNCTION(buffer_append);

/* IP functions. */
VM_DECLARE_FUNCTION(make_client);
VM_DECLARE_FUNCTION(make_server);
VM_DECLARE_FUNCTION(peer_name);
VM_DECLARE_FUNCTION(accept_client);
VM_DECLARE_FUNCTION(incoming_clientp);
VM_DECLARE_FUNCTION(addr_to_string);
VM_DECLARE_FUNCTION(resolve_hostname);

/* Mathematical functions using floats. */
VM_DECLARE_FUNCTION(floor);
VM_DECLARE_FUNCTION(ceiling);
VM_DECLARE_FUNCTION(round);
VM_DECLARE_FUNCTION(truncate);
VM_DECLARE_FUNCTION(exp);
VM_DECLARE_FUNCTION(log);
VM_DECLARE_FUNCTION(sin);
VM_DECLARE_FUNCTION(cos);
VM_DECLARE_FUNCTION(tan);
VM_DECLARE_FUNCTION(asin);
VM_DECLARE_FUNCTION(acos);
VM_DECLARE_FUNCTION(atan);
VM_DECLARE_FUNCTION(sqrt);
VM_DECLARE_FUNCTION(expt);
VM_DECLARE_FUNCTION(exact_to_inexact);
VM_DECLARE_FUNCTION(inexact_to_exact);

/* Bit operations. */
VM_DECLARE_FUNCTION(bit_and);
VM_DECLARE_FUNCTION(bit_or);
VM_DECLARE_FUNCTION(bit_invert);
VM_DECLARE_FUNCTION(bit_not);
VM_DECLARE_FUNCTION(bit_xor);
VM_DECLARE_FUNCTION(bit_shift);

/* Packet management functions. */
VM_DECLARE_FUNCTION(construct_packet);
VM_DECLARE_FUNCTION(deconstruct_packet);

#endif /* !VM_FUNCTIONS_H */
