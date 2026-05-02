# Copyright (c) 2026, RISE Research Institutes of Sweden AB
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived
#    from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.

"""
VeloxVM Primitive Operations Mapping

This module defines the exact mapping of VeloxVM's built-in operators to
their symbol IDs. The order must match core/vm-procedures.c exactly; a
mismatch shifts every symbol ID after the divergence and breaks bytecode
compatibility.
"""

# Complete list of VM primitives in exact order (index = symbol ID).
# When adding a primitive, insert it in the same position it occupies in
# core/vm-procedures.c's operators[] table.
VM_PRIMITIVES = [
    # Mathematical functions
    'add', 'subtract', 'multiply', 'divide', 'gcd', 'lcm',
    'numerator', 'denominator', 'quotient', 'remainder', 'modulo',

    # Comparison functions
    'equal', 'different', 'less_than', 'less_than_equal',
    'greater_than', 'greater_than_equal', 'zerop',

    # Primitive functions
    'bind', 'bind_function', 'return', 'begin', 'if', 'define', 'set',
    'and', 'or', 'apply', 'quote',

    # Type predicates
    'numberp', 'integerp', 'rationalp', 'realp', 'complexp',
    'exactp', 'inexactp', 'procedurep', 'booleanp', 'portp',
    'not', 'eqp', 'eqvp', 'equalp',

    # System functions
    'system_info', 'load_program', 'import', 'get_devices',
    'print', 'random', 'time', 'get_programs', 'program_info', 'exit',

    # List functions
    'list', 'cons', 'push', 'pop', 'car', 'cdr',
    'list_ref', 'list_tail', 'slice', 'append', 'remove', 'reverse', 'length',
    'nullp', 'listp', 'pairp', 'set_car', 'set_cdr',
    'memq', 'memv', 'member', 'assq', 'assv', 'assoc',
    'list_enumerate', 'list_zip', 'list_index',

    # Higher-order list functions
    'map', 'filter', 'for_each', 'reduce', 'count',

    # Character functions
    'charp', 'char_compare', 'char_class', 'char_to_integer',
    'integer_to_char', 'char_upcase', 'char_downcase',

    # String functions
    'make_string', 'string', 'stringp', 'string_length',
    'string_ref', 'string_set', 'string_to_list', 'list_to_string',
    'vector_to_string', 'string_fill', 'string_compare', 'substring',
    'string_append', 'string_copy', 'string_split', 'string_join',
    'number_to_string', 'string_to_number',

    # Exception functions
    'guard', 'raise',

    # Thread functions
    'thread_create', 'thread_fork', 'thread_id', 'thread_join',
    'thread_sleep', 'thread_specific', 'thread_specific_set',
    'thread_terminate', 'thread_yield', 'thread_stats',

    # Mutex functions
    'mutexp', 'make_mutex', 'mutex_name', 'mutex_specific',
    'mutex_specific_set', 'mutex_state', 'mutex_lock', 'mutex_unlock',

    # Vector functions
    'make_vector', 'vector', 'vectorp', 'bufferp', 'vector_merge',
    'vector_length', 'vector_ref', 'vector_set', 'vector_to_list',
    'list_to_vector', 'vector_fill', 'make_buffer', 'buffer_append',

    # Higher-order vector functions
    'vector_for_each', 'vector_count', 'vector_fold', 'vector_map',

    # I/O functions
    'input_portp', 'output_portp', 'current_input_port', 'current_output_port',
    'open_input_file', 'open_output_file', 'close_input_port', 'close_output_port',
    'read_char', 'read', 'peek_char', 'eof_objectp', 'char_readyp',
    'write_char', 'write', 'display',
    'with_input_from_file', 'with_output_to_file',
    'make_client', 'make_server', 'peer_name', 'accept_client',
    'incoming_clientp', 'addr_to_string', 'resolve_hostname',

    # Mathematical functions using floats
    'floor', 'ceiling', 'round', 'truncate', 'exp', 'log',
    'sin', 'cos', 'tan', 'asin', 'acos', 'atan',
    'sqrt', 'expt', 'exact_to_inexact', 'inexact_to_exact',

    # Evaluation control functions
    'call_with_cc', 'values', 'call_with_values', 'dynamic_wind', 'eval',

    # Bit manipulation functions
    'bit_and', 'bit_or', 'bit_invert', 'bit_not', 'bit_xor', 'bit_shift',

    # Packet management functions
    'construct_packet', 'deconstruct_packet',

    # Additional type predicates
    'symbolp',

    # Type conversion functions
    'symbol_to_string',

    # Box operations (compiler-emitted for closure mutable captures)
    'box', 'box_ref', 'box_set',

    # Variadic-lambda binding (compiler-emitted; mirrors bind_function but
    # the last formal soaks up extras as a list)
    'bind_function_rest',

    # R7RS string->symbol (restricted form; looks up existing symbols only)
    'string_to_symbol',
]

# Create reverse lookup dictionary (name -> ID)
_PRIMITIVE_MAP = {name: idx for idx, name in enumerate(VM_PRIMITIVES)}

# Scheme-style aliases for common operations (map Scheme names to Python-friendly names)
SCHEME_ALIASES = {
    '+': 'add',
    '-': 'subtract',
    '*': 'multiply',
    '/': 'divide',
    '=': 'equal',
    '!=': 'different',
    '/=': 'different',
    '<': 'less_than',
    '<=': 'less_than_equal',
    '>': 'greater_than',
    '>=': 'greater_than_equal',
    'zero?': 'zerop',
    'number?': 'numberp',
    'integer?': 'integerp',
    'rational?': 'rationalp',
    'real?': 'realp',
    'complex?': 'complexp',
    'exact?': 'exactp',
    'inexact?': 'inexactp',
    'procedure?': 'procedurep',
    'boolean?': 'booleanp',
    'port?': 'portp',
    'eq?': 'eqp',
    'eqv?': 'eqvp',
    'equal?': 'equalp',
    'null?': 'nullp',
    'list?': 'listp',
    'pair?': 'pairp',
    'char?': 'charp',
    'string?': 'stringp',
    'vector?': 'vectorp',
    'buffer?': 'bufferp',
    'mutex?': 'mutexp',
    'symbol?': 'symbolp',
    'input-port?': 'input_portp',
    'output-port?': 'output_portp',
    'eof-object?': 'eof_objectp',
    'char-ready?': 'char_readyp',
    'set!': 'set',
    'set-car!': 'set_car',
    'set-cdr!': 'set_cdr',
    'string-set!': 'string_set',
    'vector-set!': 'vector_set',
    'list-ref': 'list_ref',
    'list-tail': 'list_tail',
    'slice': 'slice',
    'for-each': 'for_each',
    'string-length': 'string_length',
    'string-ref': 'string_ref',
    'string->list': 'string_to_list',
    'list->string': 'list_to_string',
    'vector->string': 'vector_to_string',
    'string-fill!': 'string_fill',
    'string-compare': 'string_compare',
    'string-append': 'string_append',
    'string-copy': 'string_copy',
    'string-split': 'string_split',
    'number->string': 'number_to_string',
    'string->number': 'string_to_number',
    'char->integer': 'char_to_integer',
    'integer->char': 'integer_to_char',
    'char-upcase': 'char_upcase',
    'char-downcase': 'char_downcase',
    'char-compare': 'char_compare',
    'char-class': 'char_class',
    'make-string': 'make_string',
    'make-vector': 'make_vector',
    'make-buffer': 'make_buffer',
    'make-mutex': 'make_mutex',
    'make-client': 'make_client',
    'make-server': 'make_server',
    'vector-ref': 'vector_ref',
    'vector-length': 'vector_length',
    'vector->list': 'vector_to_list',
    'list->vector': 'list_to_vector',
    'vector-fill!': 'vector_fill',
    'vector-merge': 'vector_merge',
    'vector-for-each': 'vector_for_each',
    'vector-count': 'vector_count',
    'vector-fold': 'vector_fold',
    'vector-map': 'vector_map',
    'buffer-append': 'buffer_append',
    'mutex-name': 'mutex_name',
    'mutex-specific': 'mutex_specific',
    'mutex-specific-set!': 'mutex_specific_set',
    'mutex-state': 'mutex_state',
    'mutex-lock!': 'mutex_lock',
    'mutex-unlock!': 'mutex_unlock',
    'thread-create!': 'thread_create',
    'thread-fork': 'thread_fork',
    'thread-id': 'thread_id',
    'thread-join!': 'thread_join',
    'thread-sleep!': 'thread_sleep',
    'thread-specific': 'thread_specific',
    'thread-specific-set!': 'thread_specific_set',
    'thread-terminate!': 'thread_terminate',
    'thread-yield!': 'thread_yield',
    'thread-stats': 'thread_stats',
    'current-input-port': 'current_input_port',
    'current-output-port': 'current_output_port',
    'open-input-file': 'open_input_file',
    'open-output-file': 'open_output_file',
    'close-input-port': 'close_input_port',
    'close-output-port': 'close_output_port',
    'read-char': 'read_char',
    'peek-char': 'peek_char',
    'write-char': 'write_char',
    'with-input-from-file': 'with_input_from_file',
    'with-output-to-file': 'with_output_to_file',
    'peer-name': 'peer_name',
    'accept-client': 'accept_client',
    'incoming-client?': 'incoming_clientp',
    'addr->string': 'addr_to_string',
    'resolve-hostname': 'resolve_hostname',
    'system-info': 'system_info',
    'load-program': 'load_program',
    'get-devices': 'get_devices',
    'get-programs': 'get_programs',
    'program-info': 'program_info',
    'exact->inexact': 'exact_to_inexact',
    'inexact->exact': 'inexact_to_exact',
    'call/cc': 'call_with_cc',
    'call-with-current-continuation': 'call_with_cc',
    'call-with-values': 'call_with_values',
    'dynamic-wind': 'dynamic_wind',
    'bit-and': 'bit_and',
    'bit-or': 'bit_or',
    'bit-invert': 'bit_invert',
    'bit-not': 'bit_not',
    'bit-xor': 'bit_xor',
    'bit-shift': 'bit_shift',
    'box-ref': 'box_ref',
    'box-set!': 'box_set',
    'construct-packet': 'construct_packet',
    'deconstruct-packet': 'deconstruct_packet',
    'symbol->string': 'symbol_to_string',
}


def get_primitive_id(name: str) -> int:
    """
    Get the symbol ID for a VM primitive by name.

    Args:
        name: The primitive name (e.g., 'add', '+', 'list-ref')

    Returns:
        The symbol ID (0-205) or None if not a primitive
    """
    # Check if it's a Scheme alias
    if name in SCHEME_ALIASES:
        name = SCHEME_ALIASES[name]

    return _PRIMITIVE_MAP.get(name)


def is_primitive(name: str) -> bool:
    """
    Check if a name is a VM primitive.

    Args:
        name: The name to check

    Returns:
        True if it's a primitive, False otherwise
    """
    return get_primitive_id(name) is not None


def get_primitive_name(symbol_id: int) -> str:
    """
    Get the primitive name for a given symbol ID.

    Args:
        symbol_id: The symbol ID (0-205)

    Returns:
        The primitive name or None if invalid ID
    """
    if 0 <= symbol_id < len(VM_PRIMITIVES):
        return VM_PRIMITIVES[symbol_id]
    return None


# The primitive count must match core/vm-procedures.c exactly.
assert len(VM_PRIMITIVES) == 206, f"Expected 206 primitives, got {len(VM_PRIMITIVES)}"
