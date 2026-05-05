"""VM REPL service client.

The VM speaks the binary frame protocol from ``velox_repl.protocol``.
This client wraps a child subprocess and exchanges frames on its
stdin/stdout. ``_send`` and ``_recv`` are the only points that touch
the byte stream, so a different transport can substitute them and
reuse everything else.

Frame flow during one REPL turn::

    driver -> APPLY(delta)
    driver <- ACK(start_id)
    driver -> RUN(entry_expr_id)
    driver <- IO_OUT(...)         (zero or more, async during evaluation)
    driver <- STATUS(...)         (optional, indicates background threads)
    driver <- RESULT(obj) | ERROR(type, msg)

``run`` returns when one of RESULT/ERROR arrives; IO_OUT frames are
dispatched to a caller-supplied callback as they're received.
"""

from __future__ import annotations

import struct
import subprocess
from dataclasses import dataclass
from typing import BinaryIO, Callable, Optional, Sequence, Union

from velox_repl.protocol import (
    Frame,
    FrameType,
    encode_frame,
    read_frame,
)


class VmError(RuntimeError):
    """The VM signaled an error while applying a delta or running a form.
    Recoverable: the next REPL turn can proceed."""


class VmCrash(RuntimeError):
    """The VM died or sent an unparseable frame. The driver must reset
    its sync state before continuing."""


@dataclass
class RunSuccess:
    obj_encoding: bytes
    threads_running: int = 0


@dataclass
class RunFailure:
    error_type: int
    message: str
    threads_running: int = 0


RunOutcome = Union[RunSuccess, RunFailure]


IoCallback = Callable[[bytes], None]
StatusCallback = Callable[[int], None]   # threads_running


class VmClient:
    """Subprocess-stdio transport. Constructor signature mirrors what
    a future Unix-socket or UDP transport would take; just swap the
    byte sink/source then."""

    def __init__(self, argv: Sequence[str], *, name: str = "vm"):
        self.argv = list(argv)
        self.name = name
        self._proc: Optional[subprocess.Popen[bytes]] = None
        self._stdin: Optional[BinaryIO] = None
        self._stdout: Optional[BinaryIO] = None

    # ---- lifecycle ---------------------------------------------------

    def start(self) -> None:
        if self._proc is not None:
            return
        self._proc = subprocess.Popen(
            self.argv,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=None,
        )
        assert self._proc.stdin is not None and self._proc.stdout is not None
        self._stdin = self._proc.stdin
        self._stdout = self._proc.stdout

    def stop(self) -> None:
        if self._proc is None:
            return
        try:
            if self._stdin and not self._stdin.closed:
                try:
                    self._stdin.close()
                except OSError:
                    pass
            try:
                self._proc.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                self._proc.terminate()
                try:
                    self._proc.wait(timeout=1.0)
                except subprocess.TimeoutExpired:
                    self._proc.kill()
        finally:
            self._proc = None
            self._stdin = None
            self._stdout = None

    def is_alive(self) -> bool:
        return self._proc is not None and self._proc.poll() is None

    # ---- request methods ---------------------------------------------

    def apply_delta(self, delta_bytes: bytes) -> int:
        """Send APPLY, block until ACK. Returns the ack'd start_id."""
        self._send(Frame(FrameType.APPLY, delta_bytes))
        while True:
            frame = self._recv_required()
            if frame.type == FrameType.ACK:
                if len(frame.payload) < 2:
                    raise VmCrash(f"{self.name}: malformed ACK")
                (start_id,) = struct.unpack("!H", frame.payload[:2])
                return start_id
            if frame.type == FrameType.ERROR:
                raise VmError(_decode_error(frame.payload))
            # We don't expect IO_OUT/RESULT/STATUS during APPLY, but tolerate
            # by ignoring -- the VM might emit a STATUS after a previous turn.
            if frame.type == FrameType.STATUS:
                continue
            raise VmCrash(
                f"{self.name}: unexpected frame during APPLY: {frame.type.name}"
            )

    def run(
        self,
        entry_expr_id: int,
        *,
        on_io: Optional[IoCallback] = None,
        on_status: Optional[StatusCallback] = None,
    ) -> RunOutcome:
        """Send RUN(entry), pump frames until RESULT or ERROR."""
        self._send(Frame(FrameType.RUN, struct.pack("!H", entry_expr_id)))
        threads_running = 0
        while True:
            frame = self._recv_required()
            if frame.type == FrameType.IO_OUT:
                if on_io is not None:
                    on_io(frame.payload)
                continue
            if frame.type == FrameType.STATUS:
                if len(frame.payload) >= 2:
                    (threads_running,) = struct.unpack("!H", frame.payload[:2])
                if on_status is not None:
                    on_status(threads_running)
                continue
            if frame.type == FrameType.RESULT:
                return RunSuccess(
                    obj_encoding=frame.payload,
                    threads_running=threads_running,
                )
            if frame.type == FrameType.ERROR:
                error_type, msg = _decode_error_full(frame.payload)
                return RunFailure(
                    error_type=error_type,
                    message=msg,
                    threads_running=threads_running,
                )
            raise VmCrash(
                f"{self.name}: unexpected frame during RUN: {frame.type.name}"
            )

    def reset(self) -> None:
        self._send(Frame(FrameType.RESET, b""))
        while True:
            frame = self._recv_required()
            if frame.type == FrameType.ACK:
                return
            if frame.type == FrameType.ERROR:
                raise VmError(_decode_error(frame.payload))
            if frame.type in (FrameType.STATUS, FrameType.IO_OUT):
                continue
            raise VmCrash(
                f"{self.name}: unexpected frame during RESET: {frame.type.name}"
            )

    def kill_thread(self, thread_id: int) -> None:
        self._send(Frame(FrameType.KILL, struct.pack("!H", thread_id)))

    def send_io_input(self, data: bytes) -> None:
        """Forward bytes as an IO_IN frame. The VM acks the frame but
        does not route the bytes to a (read) call; sending them is a
        no-op end-to-end."""
        self._send(Frame(FrameType.IO_IN, data))

    # ---- low-level I/O -----------------------------------------------

    def _send(self, frame: Frame) -> None:
        if not self._stdin:
            raise VmCrash(f"{self.name}: not running")
        try:
            self._stdin.write(encode_frame(frame))
            self._stdin.flush()
        except (BrokenPipeError, OSError) as e:
            raise VmCrash(f"{self.name}: send failed: {e}") from e

    def _recv_required(self) -> Frame:
        frame = self._recv()
        if frame is None:
            raise VmCrash(f"{self.name}: stdout closed unexpectedly")
        return frame

    def _recv(self) -> Optional[Frame]:
        if not self._stdout:
            raise VmCrash(f"{self.name}: not running")
        try:
            return read_frame(self._stdout)
        except IOError as e:
            raise VmCrash(f"{self.name}: receive failed: {e}") from e


def _decode_error(payload: bytes) -> str:
    _, msg = _decode_error_full(payload)
    return msg


def _decode_error_full(payload: bytes) -> tuple[int, str]:
    if len(payload) < 1:
        return (0, "")
    error_type = payload[0]
    msg = payload[1:].decode("utf-8", errors="replace")
    return (error_type, msg)
