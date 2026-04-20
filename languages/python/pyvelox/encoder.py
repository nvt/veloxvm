"""
VeloxVM Bytecode Encoding Functions

This module implements the low-level bytecode encoding based on the VeloxVM
bytecode format specification (doc/bytecode-format.md).

Encoding scheme:
- Bit 7 = 0: ATOM (data values)
- Bit 7 = 1: FORM (function calls)
"""

from typing import Optional
from .primitives import get_primitive_id
from .bytecode import Bytecode

# Token types
VM_TOKEN_ATOM = 0
VM_TOKEN_FORM = 1

# Object types (for ATOM tokens)
VM_TYPE_BOOLEAN = 0
VM_TYPE_INTEGER = 1
VM_TYPE_RATIONAL = 2
VM_TYPE_REAL = 3
VM_TYPE_STRING = 4
VM_TYPE_SYMBOL = 5
VM_TYPE_CHARACTER = 6

# Form types (for FORM tokens)
VM_FORM_INLINE = 0
VM_FORM_LAMBDA = 1
VM_FORM_REF = 2

# Symbol scopes
VM_SYMBOL_SCOPE_CORE = 0
VM_SYMBOL_SCOPE_APP = 1


def encode_boolean(value: bool) -> bytes:
    """
    Encode a boolean value.

    Format:
    - Byte 0: bit 7=0 (ATOM), bit 3=value, bits 2-0=0 (BOOLEAN)

    Args:
        value: True or False

    Returns:
        1-byte encoding
    """
    info = 1 if value else 0
    header = (VM_TOKEN_ATOM << 7) | (info << 3) | VM_TYPE_BOOLEAN
    return bytes([header])


def encode_integer(n: int) -> bytes:
    """
    Encode an integer value (variable-length: 2-5 bytes total).

    Format (actual VM implementation in core/vm-bytecode.c):
    - Byte 0: bit 7=0 (ATOM), bits 6-3=embedded, bits 2-0=1 (INTEGER)
      where embedded (bits 6-3) = (sign << 3) | byte_count
        - bit 6 = sign (0=positive, 1=negative)
        - bits 5-3 = byte count (1-4)
    - Bytes 1+: value in big-endian (1-4 bytes)

    Note: This matches the Racket compiler and actual VM implementation.
    The bytecode-format.md doc describes a different (unused) format.

    Args:
        n: Integer value (INT32_MIN to INT32_MAX)

    Returns:
        Byte encoding (2-5 bytes total)
    """
    value = abs(n)
    sign = 1 if n < 0 else 0

    # Determine how many bytes needed (1-4)
    if value == 0:
        byte_count = 1
    else:
        byte_count = max(1, (value.bit_length() + 7) // 8)

    if byte_count > 4:
        raise ValueError(f"Integer {n} too large for 32-bit encoding")

    # Embedded field: (sign << 3) | byte_count, goes into bits 6-3
    embedded = (sign << 3) | byte_count

    # Header: bit 7=0 (ATOM), bits 6-3=embedded, bits 2-0=type
    header = (VM_TOKEN_ATOM << 7) | ((embedded << 3) & 0x78) | VM_TYPE_INTEGER

    # Value bytes (big-endian)
    value_bytes = value.to_bytes(byte_count, byteorder='big')

    return bytes([header]) + value_bytes


def encode_string(s: str, bc: Bytecode) -> bytes:
    """
    Encode a string reference (string table lookup).

    Format:
    - Byte 0: bit 7=0 (ATOM), bits 2-0=4 (STRING)
    - Byte 1: bit 7=extended, bits 6-0=ID (or high 7 bits if extended)
    - Byte 2 (if extended): low 8 bits of ID

    Args:
        s: The string value
        bc: Bytecode container (for string table)

    Returns:
        2-3 byte encoding
    """
    string_id = bc.add_string(s)

    # Header byte
    header = (VM_TOKEN_ATOM << 7) | VM_TYPE_STRING

    if string_id < 128:
        # Simple form: 7-bit ID
        return bytes([header, string_id])
    else:
        # Extended form: 15-bit ID
        high = ((string_id >> 8) & 0x7F) | 0x80  # Set bit 7 (extended flag)
        low = string_id & 0xFF
        return bytes([header, high, low])


def encode_character(ch: str) -> bytes:
    """
    Encode a character.

    Format:
    - Byte 0: bit 7=0 (ATOM), bits 2-0=6 (CHARACTER)
    - Byte 1: character value (single byte)

    Args:
        ch: Single character string

    Returns:
        2-byte encoding
    """
    if len(ch) != 1:
        raise ValueError(f"Character encoding requires single character, got: {ch}")

    header = (VM_TOKEN_ATOM << 7) | VM_TYPE_CHARACTER
    return bytes([header, ord(ch)])


def encode_symbol(name: str, bc: Bytecode) -> bytes:
    """
    Encode a symbol reference.

    Format:
    - Byte 0: bit 7=0 (ATOM), bits 2-0=5 (SYMBOL)
    - Byte 1: bit 7=scope (0=core, 1=app), bit 6=extended, bits 5-0=ID (or high 6 bits)
    - Byte 2 (if extended): low 8 bits of ID

    Args:
        name: Symbol name (could be primitive or user-defined)
        bc: Bytecode container (for symbol table)

    Returns:
        2-3 byte encoding
    """
    # Check if it's a core primitive
    prim_id = get_primitive_id(name)

    if prim_id is not None:
        # Core symbol (VM primitive)
        scope = VM_SYMBOL_SCOPE_CORE
        symbol_id = prim_id
    else:
        # Application symbol (user-defined)
        scope = VM_SYMBOL_SCOPE_APP
        symbol_id = bc.add_symbol(name)

    # Header byte
    header = (VM_TOKEN_ATOM << 7) | VM_TYPE_SYMBOL

    if symbol_id < 64:
        # Simple form: 6-bit ID
        sym_byte = (scope << 7) | symbol_id
        return bytes([header, sym_byte])
    else:
        # Extended form: 14-bit ID
        high = (symbol_id >> 8) & 0x3F
        low = symbol_id & 0xFF
        sym_byte = (scope << 7) | 0x40 | high  # Set bit 6 (extended flag)
        return bytes([header, sym_byte, low])


def encode_inline_form(argc: int) -> bytes:
    """
    Encode an inline form header.

    Format:
    - Byte 0: bit 7=1 (FORM), bits 6-5=00 (INLINE), bits 5-0=argc

    Args:
        argc: Argument count (0-63), includes operator + arguments

    Returns:
        1-byte encoding
    """
    if not (0 <= argc <= 63):
        raise ValueError(f"Inline form argc must be 0-63, got {argc}")

    return bytes([0x80 | (VM_FORM_INLINE << 5) | (argc & 0x3F)])


def encode_form_lambda(expr_id: int) -> bytes:
    """
    Encode a lambda form reference.

    Format:
    - Byte 0: bit 7=1 (FORM), bits 6-5=01 (LAMBDA), bit 4=simple, bits 3-0=ID (low 4 bits)
    - Byte 1 (if not simple): high 8 bits of ID

    Args:
        expr_id: Expression table index (0-4095)

    Returns:
        1-2 byte encoding
    """
    if not (0 <= expr_id <= 4095):
        raise ValueError(f"Lambda form expr_id must be 0-4095, got {expr_id}")

    if expr_id < 16:
        # Simple form: 4-bit ID
        return bytes([0x80 | (VM_FORM_LAMBDA << 5) | 0x10 | expr_id])
    else:
        # Extended form: 12-bit ID
        high = (expr_id >> 8) & 0x0F
        low = expr_id & 0xFF
        return bytes([0x80 | (VM_FORM_LAMBDA << 5) | high, low])


def encode_form_ref(expr_id: int) -> bytes:
    """
    Encode a form reference.

    Format:
    - Byte 0: bit 7=1 (FORM), bits 6-5=10 (REF), bit 4=simple, bits 3-0=ID (low 4 bits)
    - Byte 1 (if not simple): high 8 bits of ID

    Args:
        expr_id: Expression table index (0-4095)

    Returns:
        1-2 byte encoding
    """
    if not (0 <= expr_id <= 4095):
        raise ValueError(f"Form ref expr_id must be 0-4095, got {expr_id}")

    if expr_id < 16:
        # Simple form: 4-bit ID
        return bytes([0x80 | (VM_FORM_REF << 5) | 0x10 | expr_id])
    else:
        # Extended form: 12-bit ID
        high = (expr_id >> 8) & 0x0F
        low = expr_id & 0xFF
        return bytes([0x80 | (VM_FORM_REF << 5) | high, low])


def create_inline_call(operator: str, args: list, bc: Bytecode) -> bytes:
    """
    Create an inline function call: inline(argc) + operator-symbol + arg-bytes.

    The inline form comes FIRST, then the operator, then arguments.

    If any argument is an inline form, we must store it separately
    and use a form ref to avoid nested inline forms (which creates malformed bytecode).

    Args:
        operator: The operator name (e.g., 'add', '+', 'if')
        args: List of already-encoded argument bytecode
        bc: Bytecode container

    Returns:
        Complete inline call bytecode
    """
    argc = 1 + len(args)  # operator + arguments

    result = bytearray()

    # 1. Inline form header FIRST
    result.extend(encode_inline_form(argc))

    # 2. Operator symbol
    result.extend(encode_symbol(operator, bc))

    # 3. Arguments - check each for inline forms to prevent nesting
    for arg in args:
        # Check if arg is an inline form (would create nested inlines)
        # Inline forms start with byte >= 0x80 and form_type == 0 (bits 6-5 == 00)
        if len(arg) > 0 and (arg[0] & 0x80) and ((arg[0] >> 5) & 0x03) == 0:
            # Arg is an inline form - store it separately and use a form ref
            # This prevents nested inline forms which are malformed bytecode
            arg_expr_id = bc.add_expression(arg)
            result.extend(encode_form_ref(arg_expr_id))
        else:
            # Arg is an atom, lambda, or form ref - can inline directly
            result.extend(arg)

    return bytes(result)


def create_inline_call_direct(operator_bytes: bytes, args: list, bc: Bytecode = None) -> bytes:
    """
    Create an inline function call with pre-encoded operator bytes.
    Used for lambda calls: inline(argc) + lambda-bytes + arg-bytes.

    If any argument is an inline form and bc is provided, we must store it separately
    and use a form ref to avoid nested inline forms (which creates malformed bytecode).

    Args:
        operator_bytes: Already-encoded operator (e.g., lambda form)
        args: List of already-encoded argument bytecode
        bc: Optional Bytecode container for storing nested inline forms

    Returns:
        Complete inline call bytecode
    """
    argc = 1 + len(args)  # operator + arguments

    result = bytearray()

    # 1. Inline form header FIRST
    result.extend(encode_inline_form(argc))

    # 2. Operator (already encoded)
    result.extend(operator_bytes)

    # 3. Arguments - check each for inline forms if bc is provided
    for arg in args:
        # Check if arg is an inline form (would create nested inlines)
        # Only store separately if bc is provided
        if (bc is not None and len(arg) > 0 and (arg[0] & 0x80) and
            ((arg[0] >> 5) & 0x03) == 0):
            # Arg is an inline form - store it separately and use a form ref
            arg_expr_id = bc.add_expression(arg)
            result.extend(encode_form_ref(arg_expr_id))
        else:
            # Arg is an atom, lambda, form ref, or bc not provided - inline directly
            result.extend(arg)

    return bytes(result)


def create_bind_form(params: list, body: bytes, bc: Bytecode, is_function: bool = False) -> bytes:
    """
    Create a bind (lambda) form: (bind param1 param2 ... body).

    If body starts with an inline form, we must store it separately
    and use a form ref to avoid nested inline forms (which creates malformed bytecode).

    Args:
        params: List of parameter names
        body: Compiled bytecode for the body
        bc: Bytecode container
        is_function: If True, use 'bind_function' to mark actual function boundaries.
                     This allows the return primitive to distinguish between functions
                     and control flow (while loops, let-expansion).

    Returns:
        Complete bind form bytecode
    """
    result = bytearray()

    # Inline form with argc = 1 (bind symbol) + len(params) + 1 (body)
    argc = 1 + len(params) + 1
    result.extend(encode_inline_form(argc))

    # Use 'bind_function' for actual Python functions, 'bind' for control flow
    bind_symbol = 'bind_function' if is_function else 'bind'
    result.extend(encode_symbol(bind_symbol, bc))

    # Parameter symbols
    for param in params:
        result.extend(encode_symbol(param, bc))

    # Body handling: Check if body is an inline form (would create nested inlines)
    # Inline forms start with byte >= 0x80 and form_type == 0 (bits 6-5 == 00)
    if len(body) > 0 and (body[0] & 0x80) and ((body[0] >> 5) & 0x03) == 0:
        # Body is an inline form - store it separately and use a form ref
        # This prevents nested inline forms which are malformed bytecode
        body_expr_id = bc.add_expression(body)
        result.extend(encode_form_ref(body_expr_id))
    else:
        # Body is an atom, lambda, or form ref - can inline directly
        result.extend(body)

    return bytes(result)
