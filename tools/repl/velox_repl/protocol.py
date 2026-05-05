"""Wire protocol primitives.

Two protocols live here:

* The driver <-> VM frame protocol. Each frame is a 3-byte header
  ``[type:u8][length:u16-be]`` followed by ``length`` payload bytes. A
  frame's payload format is determined by its type (see ``FrameType``).
  This protocol is transport-agnostic: identical bytes flow over stdio,
  Unix sockets, UDP, or CoAP block-wise transfer.

* The driver <-> compiler line protocol. One s-expression per line on
  stdin/stdout. The driver writes ``(compile-form "...")`` etc.; the
  compiler responds with ``(ok ...)``, ``(incomplete)`` or
  ``(error ...)``. We use a small handwritten reader/writer rather than
  pulling in sexpdata for one tiny dependency.
"""

from __future__ import annotations

import io
import struct
from dataclasses import dataclass
from enum import IntEnum
from typing import Any, BinaryIO, List, Optional, Union


class FrameType(IntEnum):
    APPLY = 0x01
    RUN = 0x02
    RESET = 0x03
    KILL = 0x04
    IO_IN = 0x05
    INFO = 0x06
    RESULT = 0x10
    ERROR = 0x11
    IO_OUT = 0x12
    STATUS = 0x13
    ACK = 0x14
    INFO_REPLY = 0x15


# Wire protocol version this driver speaks. INFO_REPLY exposes the same
# value from the VM side so a mismatch surfaces with a clear message at
# connect time.
PROTOCOL_VERSION = 1


# Capability flags reported in INFO_REPLY.
CAP_IO_OUT = 0x0001
CAP_IO_IN = 0x0002
CAP_KILL = 0x0004
CAP_STATUS = 0x0008


@dataclass
class Frame:
    type: FrameType
    payload: bytes


@dataclass
class VmInfo:
    """Decoded INFO_REPLY payload."""
    protocol_version: int
    bytecode_version: int
    capabilities: int
    name: str
    version: str
    build: str

    def supports(self, capability: int) -> bool:
        return bool(self.capabilities & capability)


def decode_info_reply(payload: bytes) -> VmInfo:
    if len(payload) < 5:
        raise IOError("INFO_REPLY too short")
    proto = payload[0]
    bcv = payload[1]
    caps = (payload[2] << 8) | payload[3]
    pos = 4
    fields = []
    for _ in range(3):  # name, version, build
        if pos >= len(payload):
            raise IOError("INFO_REPLY truncated")
        n = payload[pos]
        pos += 1
        if pos + n > len(payload):
            raise IOError("INFO_REPLY field truncated")
        fields.append(payload[pos:pos + n].decode("utf-8", errors="replace"))
        pos += n
    return VmInfo(
        protocol_version=proto,
        bytecode_version=bcv,
        capabilities=caps,
        name=fields[0],
        version=fields[1],
        build=fields[2],
    )


def encode_info_reply(info: VmInfo) -> bytes:
    parts = bytearray()
    parts.append(info.protocol_version & 0xFF)
    parts.append(info.bytecode_version & 0xFF)
    parts.append((info.capabilities >> 8) & 0xFF)
    parts.append(info.capabilities & 0xFF)
    for s in (info.name, info.version, info.build):
        b = s.encode("utf-8")
        if len(b) > 255:
            b = b[:255]
        parts.append(len(b))
        parts.extend(b)
    return bytes(parts)


_FRAME_HEADER = struct.Struct("!BH")
MAX_PAYLOAD = 0xFFFF


def encode_frame(frame: Frame) -> bytes:
    if len(frame.payload) > MAX_PAYLOAD:
        raise ValueError(f"frame payload too large: {len(frame.payload)} bytes")
    return _FRAME_HEADER.pack(int(frame.type), len(frame.payload)) + frame.payload


def read_frame(stream: BinaryIO) -> Optional[Frame]:
    """Read one frame from ``stream``. Returns None on clean EOF."""
    header = _read_exactly(stream, _FRAME_HEADER.size)
    if header is None:
        return None
    ftype, length = _FRAME_HEADER.unpack(header)
    payload = b""
    if length:
        chunk = _read_exactly(stream, length)
        if chunk is None:
            raise IOError("short read on frame payload")
        payload = chunk
    try:
        type_enum = FrameType(ftype)
    except ValueError as e:
        raise IOError(f"unknown frame type 0x{ftype:02x}") from e
    return Frame(type_enum, payload)


def _read_exactly(stream: BinaryIO, n: int) -> Optional[bytes]:
    out = bytearray()
    while len(out) < n:
        chunk = stream.read(n - len(out))
        if not chunk:
            return None if not out else bytes(out)
        out.extend(chunk)
    return bytes(out)


# ---------------------------------------------------------------------------
# S-expression line protocol
# ---------------------------------------------------------------------------


class Symbol(str):
    """A bare identifier in an s-expression. Subclass of str so equality
    against a literal string still works for tag dispatch."""

    __slots__ = ()

    def __repr__(self) -> str:
        return f"Symbol({super().__repr__()})"


SExp = Union[Symbol, str, int, List["SExp"]]


class SExpError(ValueError):
    pass


def parse_sexp(text: str) -> SExp:
    reader = _SExpReader(text)
    value = reader.read()
    reader.skip_ws()
    if not reader.eof():
        raise SExpError(f"trailing input at offset {reader.pos}")
    return value


def format_sexp(value: SExp) -> str:
    if isinstance(value, Symbol):
        return str(value)
    if isinstance(value, str):
        return _quote_string(value)
    if isinstance(value, bool):
        # Must come before int because bool is a subclass of int.
        return "#t" if value else "#f"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, list):
        return "(" + " ".join(format_sexp(v) for v in value) + ")"
    if value is None:
        return "()"
    raise SExpError(f"cannot format value of type {type(value).__name__}")


def _looks_like_float(token: str) -> bool:
    has_digit = any(c.isdigit() for c in token)
    has_marker = "." in token or "e" in token or "E" in token
    return has_digit and has_marker


def _quote_string(s: str) -> str:
    out = ['"']
    for ch in s:
        if ch == "\\":
            out.append("\\\\")
        elif ch == '"':
            out.append('\\"')
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\r":
            out.append("\\r")
        elif ch == "\t":
            out.append("\\t")
        elif ord(ch) < 0x20:
            out.append(f"\\x{ord(ch):02x};")
        else:
            out.append(ch)
    out.append('"')
    return "".join(out)


class _SExpReader:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0

    def eof(self) -> bool:
        return self.pos >= len(self.text)

    def peek(self) -> str:
        return self.text[self.pos] if self.pos < len(self.text) else ""

    def advance(self) -> str:
        ch = self.text[self.pos]
        self.pos += 1
        return ch

    def skip_ws(self) -> None:
        while not self.eof():
            ch = self.peek()
            if ch.isspace():
                self.pos += 1
            elif ch == ";":
                while not self.eof() and self.peek() != "\n":
                    self.pos += 1
            else:
                return

    def read(self) -> SExp:
        self.skip_ws()
        if self.eof():
            raise SExpError("unexpected EOF")
        ch = self.peek()
        if ch == "(":
            return self._read_list()
        if ch == '"':
            return self._read_string()
        if ch == "#":
            return self._read_hash()
        return self._read_atom()

    def _read_list(self) -> List[SExp]:
        assert self.advance() == "("
        items: List[SExp] = []
        while True:
            self.skip_ws()
            if self.eof():
                raise SExpError("unterminated list")
            if self.peek() == ")":
                self.pos += 1
                return items
            items.append(self.read())

    def _read_string(self) -> str:
        assert self.advance() == '"'
        out: List[str] = []
        while True:
            if self.eof():
                raise SExpError("unterminated string")
            ch = self.advance()
            if ch == '"':
                return "".join(out)
            if ch == "\\":
                if self.eof():
                    raise SExpError("trailing backslash")
                esc = self.advance()
                if esc == "n":
                    out.append("\n")
                elif esc == "r":
                    out.append("\r")
                elif esc == "t":
                    out.append("\t")
                elif esc == "\\":
                    out.append("\\")
                elif esc == '"':
                    out.append('"')
                elif esc == "x":
                    hex_chars: List[str] = []
                    while not self.eof() and self.peek() != ";":
                        hex_chars.append(self.advance())
                    if self.eof():
                        raise SExpError("unterminated \\x escape")
                    self.advance()  # consume ';'
                    out.append(chr(int("".join(hex_chars), 16)))
                else:
                    raise SExpError(f"unknown escape \\{esc}")
            else:
                out.append(ch)

    def _read_hash(self) -> SExp:
        assert self.advance() == "#"
        if self.eof():
            raise SExpError("dangling #")
        ch = self.advance()
        if ch == "t":
            return Symbol("#t")
        if ch == "f":
            return Symbol("#f")
        raise SExpError(f"unsupported #-form: #{ch}")

    def _read_atom(self) -> SExp:
        start = self.pos
        while not self.eof():
            ch = self.peek()
            if ch.isspace() or ch in "()\";":
                break
            self.pos += 1
        token = self.text[start:self.pos]
        if not token:
            raise SExpError(f"empty atom at offset {start}")
        # Try integer, then float.
        try:
            return int(token)
        except ValueError:
            pass
        if _looks_like_float(token):
            try:
                return float(token)
            except ValueError:
                pass
        return Symbol(token)


def read_sexp_line(stream: io.TextIOBase) -> Optional[SExp]:
    """Read one s-expression terminated by a newline at top level.

    The compiler may emit multi-line responses for readability, but each
    response always ends with a newline that closes the top-level form.
    We accumulate lines until paren depth reaches zero outside of strings.
    """
    buf: List[str] = []
    depth = 0
    in_string = False
    escape = False
    while True:
        line = stream.readline()
        if not line:
            return None if not buf else parse_sexp("".join(buf))
        buf.append(line)
        for ch in line:
            if escape:
                escape = False
                continue
            if in_string:
                if ch == "\\":
                    escape = True
                elif ch == '"':
                    in_string = False
                continue
            if ch == '"':
                in_string = True
            elif ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
        if depth == 0 and any(c not in " \t\r\n" for c in "".join(buf)):
            return parse_sexp("".join(buf))


def write_sexp_line(stream: io.TextIOBase, value: SExp) -> None:
    stream.write(format_sexp(value))
    stream.write("\n")
    stream.flush()
