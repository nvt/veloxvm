"""Stub-only delta format used between stub_compiler and stub_vm.

We can't ship the real VeloxVM bytecode delta format because the real
append-loader doesn't exist yet. Instead, the stub compiler bakes the
*precomputed* result into the delta and the stub VM just hands it back
on RUN. That's enough for the driver to drive both peers end-to-end and
exercise the protocol.

Layout (network byte order, all sizes in bytes):

    0..1    magic           0x5E 0xB6   (matches the planned real format)
    2       version         0x01
    3       stub_marker     0xFF        (real VM rejects; stub VM accepts)
    4..5    entry_expr_id   uint16
    6       kind            0=expr 1=stmt 2=define
    7..8    name_len        uint16
    9..     name            UTF-8 bytes (name_len bytes)
    next..  obj_encoding    variable, see velox_repl.render.decode

The real delta format is described in doc/repl-design.md (forthcoming)
and shares only the magic + version prefix.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass


MAGIC = b"\x5e\xb6"
VERSION = 0x01
STUB_MARKER = 0xFF

KIND_EXPR = 0
KIND_STMT = 1
KIND_DEFINE = 2

KIND_NAMES = {KIND_EXPR: "expr", KIND_STMT: "stmt", KIND_DEFINE: "define"}
KIND_TAGS = {v: k for k, v in KIND_NAMES.items()}


@dataclass
class StubDelta:
    entry_expr_id: int
    kind: str
    name: str
    obj_encoding: bytes


def encode(delta: StubDelta) -> bytes:
    name_bytes = delta.name.encode("utf-8")
    header = (
        MAGIC
        + bytes([VERSION, STUB_MARKER])
        + struct.pack("!H", delta.entry_expr_id)
        + bytes([KIND_TAGS[delta.kind]])
        + struct.pack("!H", len(name_bytes))
        + name_bytes
    )
    return header + delta.obj_encoding


def decode(data: bytes) -> StubDelta:
    if len(data) < 9:
        raise ValueError("stub delta too short")
    if data[0:2] != MAGIC:
        raise ValueError(f"bad magic: {data[0:2]!r}")
    if data[2] != VERSION:
        raise ValueError(f"bad version: {data[2]}")
    if data[3] != STUB_MARKER:
        raise ValueError(
            "delta is not in stub format -- was a real-compiler delta sent "
            "to the stub VM?"
        )
    (entry_expr_id,) = struct.unpack("!H", data[4:6])
    kind_tag = data[6]
    if kind_tag not in KIND_NAMES:
        raise ValueError(f"unknown kind tag: {kind_tag}")
    (name_len,) = struct.unpack("!H", data[7:9])
    if 9 + name_len > len(data):
        raise ValueError("stub delta truncated")
    name = data[9 : 9 + name_len].decode("utf-8", errors="replace")
    obj_encoding = data[9 + name_len :]
    return StubDelta(
        entry_expr_id=entry_expr_id,
        kind=KIND_NAMES[kind_tag],
        name=name,
        obj_encoding=obj_encoding,
    )
