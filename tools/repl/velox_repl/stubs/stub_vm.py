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

from velox_repl.protocol import (
    Frame,
    FrameType,
    encode_frame,
    read_frame,
)
from velox_repl.stubs.stub_delta import StubDelta, decode as decode_delta


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
                delta = decode_delta(frame.payload)
            except ValueError as e:
                _write(stdout, _err_frame(2, f"bad delta: {e}"))
                continue
            program[delta.entry_expr_id] = (delta.kind, delta.name, delta.obj_encoding)
            ack_id = max(next_expected_start, delta.entry_expr_id)
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
