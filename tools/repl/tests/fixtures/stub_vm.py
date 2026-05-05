"""In-process stand-in for VeloxVM's REPL service.

Speaks the real driver-VM frame protocol on stdin/stdout but doesn't
actually run any bytecode. It pulls precomputed results from stub-format
deltas (see ``stub_delta``) and hands them back on RUN.

This is enough to exercise the driver's compile -> apply -> run -> render
pipeline end-to-end. When the real C-side append-loader exists, it
replaces this binary; the protocol on the wire is identical.
"""

from __future__ import annotations

import struct
import sys
from typing import Dict, Tuple

import struct as _struct

from velox_repl.protocol import (
    Frame,
    FrameType,
    encode_frame,
    read_frame,
)
from velox_repl.render.decode import encode_string
from tests.fixtures.stub_delta import (
    MAGIC,
    STUB_MARKER,
    StubDelta,
    decode as decode_delta,
)


# Real-format delta tags from doc/repl-design.md
_TAG_STRINGS = 0x01
_TAG_SYMBOLS = 0x02
_TAG_EXPRS = 0x03
_TAG_CAPTURES = 0x04
_TAG_ENTRY = 0x05
_TAG_END = 0xFF


def _is_stub_delta(data: bytes) -> bool:
    return (
        len(data) >= 4
        and data[0:2] == MAGIC
        and data[3] == STUB_MARKER
    )


def _summarize_real_delta(data: bytes) -> tuple[int, str]:
    """Inspect a real-format delta and return (entry_expr_id, summary).
    The stub doesn't actually execute; it just hands back a synthetic
    description so the driver pipeline can be exercised end-to-end."""
    if len(data) < 3 or data[0:2] != MAGIC:
        raise ValueError(f"bad magic: {data[0:2]!r}")
    version = data[2]
    if version != 1:
        raise ValueError(f"unsupported delta version: {version}")
    counts = {"strings": 0, "symbols": 0, "exprs": 0, "captures": 0}
    entry = 0
    pos = 3
    while pos < len(data):
        tag = data[pos]
        if pos + 3 > len(data):
            raise ValueError("truncated section header")
        (plen,) = _struct.unpack("!H", data[pos + 1 : pos + 3])
        payload = data[pos + 3 : pos + 3 + plen]
        if len(payload) < plen:
            raise ValueError("truncated section payload")
        if tag == _TAG_END:
            break
        if tag in (_TAG_STRINGS, _TAG_SYMBOLS, _TAG_EXPRS):
            if len(payload) >= 4:
                (_, count) = _struct.unpack("!HH", payload[:4])
                key = {
                    _TAG_STRINGS: "strings",
                    _TAG_SYMBOLS: "symbols",
                    _TAG_EXPRS: "exprs",
                }[tag]
                counts[key] += count
        elif tag == _TAG_CAPTURES:
            if len(payload) >= 2:
                (count,) = _struct.unpack("!H", payload[:2])
                counts["captures"] += count
        elif tag == _TAG_ENTRY:
            if len(payload) >= 2:
                (entry,) = _struct.unpack("!H", payload[:2])
        pos += 3 + plen
    summary = "; ".join(
        f"{k}+{v}" for k, v in counts.items() if v
    ) or "no new items"
    return entry, f"<real delta: {summary}, entry={entry}>"


def main() -> int:
    stdin = sys.stdin.buffer
    stdout = sys.stdout.buffer

    # entry_expr_id -> (kind, name, obj_encoding)
    program: Dict[int, Tuple[str, str, bytes]] = {}
    next_expected_start: int = 1

    while True:
        try:
            frame = read_frame(stdin)
        except IOError as e:
            _write(stdout, _err_frame(0, f"frame read error: {e}"))
            return 1
        if frame is None:
            return 0

        if frame.type == FrameType.APPLY:
            try:
                if _is_stub_delta(frame.payload):
                    delta = decode_delta(frame.payload)
                    program[delta.entry_expr_id] = (
                        delta.kind, delta.name, delta.obj_encoding,
                    )
                    ack_id = max(next_expected_start, delta.entry_expr_id)
                else:
                    # Real-format delta. We don't execute, but we record
                    # a synthetic placeholder result keyed by entry id so
                    # RUN works end-to-end against a real compiler.
                    entry, summary = _summarize_real_delta(frame.payload)
                    program[entry] = ("expr", "", encode_string(summary))
                    ack_id = max(next_expected_start, entry)
            except ValueError as e:
                _write(stdout, _err_frame(2, f"bad delta: {e}"))
                continue
            next_expected_start = ack_id + 1
            _write(stdout, Frame(FrameType.ACK, struct.pack("!H", ack_id)))
            continue

        if frame.type == FrameType.RUN:
            if len(frame.payload) < 2:
                _write(stdout, _err_frame(7, "RUN payload too short"))
                continue
            (entry,) = struct.unpack("!H", frame.payload[:2])
            entry_info = program.get(entry)
            if entry_info is None:
                _write(stdout, _err_frame(7, f"no expression at id {entry}"))
                continue
            _kind, _name, obj_encoding = entry_info
            _write(stdout, Frame(FrameType.RESULT, obj_encoding))
            continue

        if frame.type == FrameType.RESET:
            program.clear()
            next_expected_start = 1
            _write(stdout, Frame(FrameType.ACK, struct.pack("!H", 0)))
            continue

        if frame.type == FrameType.KILL:
            _write(stdout, Frame(FrameType.ACK, struct.pack("!H", 0)))
            continue

        if frame.type == FrameType.IO_IN:
            # The stub doesn't model application input; just discard.
            continue

        _write(stdout, _err_frame(0, f"unexpected frame type {frame.type.name}"))


def _write(stdout, frame: Frame) -> None:
    stdout.write(encode_frame(frame))
    stdout.flush()


def _err_frame(error_type: int, msg: str) -> Frame:
    payload = bytes([error_type]) + msg.encode("utf-8")
    return Frame(FrameType.ERROR, payload)


if __name__ == "__main__":
    sys.exit(main())
