"""Decoder for the VM's runtime-value wire encoding.

Each value is a tag byte followed by a tag-specific payload. This is a
*driver-side* protocol, not the VM's internal ``vm_obj_t`` layout -- the
VM serializes its tagged objects into this stable shape inside ``RESULT``
frames so the driver doesn't need to depend on internal C structs.

Format:

    0x01  NONE      (no payload)
    0x02  BOOLEAN   1B value (0/1)
    0x03  INTEGER   8B signed big-endian
    0x04  REAL      8B IEEE 754 big-endian
    0x05  CHAR      4B Unicode codepoint big-endian
    0x06  STRING    2B length + UTF-8 bytes
    0x07  SYMBOL    2B length + UTF-8 bytes
    0x08  LIST      2B count + count * sub-value
    0x09  VECTOR    2B count + count * sub-value
    0x0A  PAIR      car + cdr (improper lists / dotted pairs)
    0x0B  RATIONAL  8B numerator + 8B denominator, both signed big-endian
    0xF0  OPAQUE    2B length + UTF-8 descriptor (closures, ports, ...)

This is a v1 schema; extend as VM types come online.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass
from typing import Any, List


@dataclass
class VObj:
    kind: str
    value: Any


_TAG_NONE = 0x01
_TAG_BOOL = 0x02
_TAG_INT = 0x03
_TAG_REAL = 0x04
_TAG_CHAR = 0x05
_TAG_STRING = 0x06
_TAG_SYMBOL = 0x07
_TAG_LIST = 0x08
_TAG_VECTOR = 0x09
_TAG_PAIR = 0x0A
_TAG_RATIONAL = 0x0B
_TAG_OPAQUE = 0xF0


class DecodeError(ValueError):
    pass


def decode(data: bytes) -> VObj:
    cursor = _Cursor(data)
    value = _read(cursor)
    if cursor.pos != len(data):
        raise DecodeError(f"trailing bytes in obj encoding: {len(data) - cursor.pos}")
    return value


class _Cursor:
    def __init__(self, data: bytes):
        self.data = data
        self.pos = 0

    def take(self, n: int) -> bytes:
        if self.pos + n > len(self.data):
            raise DecodeError("short read in obj encoding")
        chunk = self.data[self.pos : self.pos + n]
        self.pos += n
        return chunk


def _read(c: _Cursor) -> VObj:
    tag = c.take(1)[0]
    if tag == _TAG_NONE:
        return VObj("none", None)
    if tag == _TAG_BOOL:
        return VObj("bool", bool(c.take(1)[0]))
    if tag == _TAG_INT:
        (v,) = struct.unpack("!q", c.take(8))
        return VObj("int", v)
    if tag == _TAG_REAL:
        (v,) = struct.unpack("!d", c.take(8))
        return VObj("real", v)
    if tag == _TAG_CHAR:
        (cp,) = struct.unpack("!I", c.take(4))
        return VObj("char", chr(cp))
    if tag == _TAG_STRING:
        (n,) = struct.unpack("!H", c.take(2))
        return VObj("string", c.take(n).decode("utf-8", errors="replace"))
    if tag == _TAG_SYMBOL:
        (n,) = struct.unpack("!H", c.take(2))
        return VObj("symbol", c.take(n).decode("utf-8", errors="replace"))
    if tag == _TAG_LIST:
        (n,) = struct.unpack("!H", c.take(2))
        items: List[VObj] = [_read(c) for _ in range(n)]
        return VObj("list", items)
    if tag == _TAG_VECTOR:
        (n,) = struct.unpack("!H", c.take(2))
        items = [_read(c) for _ in range(n)]
        return VObj("vector", items)
    if tag == _TAG_PAIR:
        car = _read(c)
        cdr = _read(c)
        return VObj("pair", (car, cdr))
    if tag == _TAG_RATIONAL:
        (num,) = struct.unpack("!q", c.take(8))
        (den,) = struct.unpack("!q", c.take(8))
        return VObj("rational", (num, den))
    if tag == _TAG_OPAQUE:
        (n,) = struct.unpack("!H", c.take(2))
        return VObj("opaque", c.take(n).decode("utf-8", errors="replace"))
    raise DecodeError(f"unknown obj tag 0x{tag:02x}")


# Helpers used by the stubs to construct sample encodings -------------


def encode_int(v: int) -> bytes:
    return bytes([_TAG_INT]) + struct.pack("!q", v)


def encode_real(v: float) -> bytes:
    return bytes([_TAG_REAL]) + struct.pack("!d", v)


def encode_bool(v: bool) -> bytes:
    return bytes([_TAG_BOOL, 1 if v else 0])


def encode_none() -> bytes:
    return bytes([_TAG_NONE])


def encode_string(s: str) -> bytes:
    b = s.encode("utf-8")
    return bytes([_TAG_STRING]) + struct.pack("!H", len(b)) + b


def encode_symbol(s: str) -> bytes:
    b = s.encode("utf-8")
    return bytes([_TAG_SYMBOL]) + struct.pack("!H", len(b)) + b


def encode_list(items: List[bytes]) -> bytes:
    return (
        bytes([_TAG_LIST])
        + struct.pack("!H", len(items))
        + b"".join(items)
    )


def encode_vector(items: List[bytes]) -> bytes:
    return (
        bytes([_TAG_VECTOR])
        + struct.pack("!H", len(items))
        + b"".join(items)
    )
