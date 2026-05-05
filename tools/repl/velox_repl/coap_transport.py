"""CoAP transport for the VM REPL service.

Talks to a Contiki-NG (or other) device running the REPL CoAP frontend.
The device exposes two resources:

  POST  <base>/cmd       request body is one driver->VM frame; response
                          body is the immediate reply frame.
  GET   <base>/events    observable; each notification carries one
                          async frame the VM produced (RESULT, ERROR,
                          IO_OUT, STATUS).

CoapVmClient implements the same duck-typed interface as the
subprocess-based VmClient (start, stop, is_alive, info, apply_delta,
run, reset, kill_thread, send_io_input). aiocoap is async, so we run
its event loop on a worker thread and bridge each sync method through
a coroutine submitted via asyncio.run_coroutine_threadsafe.
"""

from __future__ import annotations

import asyncio
import queue
import struct
import threading
from concurrent.futures import TimeoutError as FuturesTimeoutError
from typing import Optional

from aiocoap import GET, POST, Context, Message

from velox_repl.protocol import (
    Frame,
    FrameType,
    VmInfo,
    decode_info_reply,
)
from velox_repl.vm import (
    IoCallback,
    RunFailure,
    RunOutcome,
    RunSuccess,
    StatusCallback,
    VmCrash,
    VmError,
)


# How long a single CoAP request may wait for its sync reply. Long
# enough to absorb retransmissions and slow links; short enough that a
# truly dead device unblocks the user instead of hanging the REPL.
DEFAULT_REQUEST_TIMEOUT = 30.0


def _parse_frames(payload: bytes) -> list[Frame]:
    """Parse zero or more frames from a CoAP payload.

    CoAP observe is a lossy stream: multiple producer updates between
    successful round-trips are coalesced into the latest notification.
    To avoid losing frames, the device packs all pending frames into
    each notification body, and we parse them all here.
    """
    frames: list[Frame] = []
    pos = 0
    while pos < len(payload):
        if pos + 3 > len(payload):
            raise VmCrash(f"CoAP frame header truncated at offset {pos}")
        ftype = payload[pos]
        (length,) = struct.unpack("!H", payload[pos + 1:pos + 3])
        body = payload[pos + 3:pos + 3 + length]
        if len(body) < length:
            raise VmCrash(f"CoAP frame body truncated at offset {pos}")
        try:
            frames.append(Frame(FrameType(ftype), body))
        except ValueError as e:
            raise VmCrash(f"unknown CoAP frame type 0x{ftype:02x}") from e
        pos += 3 + length
    return frames


def _parse_frame(payload: bytes) -> Optional[Frame]:
    """Single-frame parse used for synchronous request responses."""
    frames = _parse_frames(payload)
    return frames[0] if frames else None


def _encode_frame(frame: Frame) -> bytes:
    payload = frame.payload
    return bytes([int(frame.type)]) + struct.pack("!H", len(payload)) + payload


class CoapVmClient:
    def __init__(self, base_uri: str, *, name: str = "vm",
                 request_timeout: float = DEFAULT_REQUEST_TIMEOUT):
        # Trim trailing slash so concatenation is consistent.
        self.base_uri = base_uri.rstrip("/")
        self.cmd_uri = self.base_uri + "/cmd"
        self.events_uri = self.base_uri + "/events"
        self.name = name
        self.request_timeout = request_timeout

        self._loop: Optional[asyncio.AbstractEventLoop] = None
        self._loop_thread: Optional[threading.Thread] = None
        self._context: Optional[Context] = None
        self._observe_task: Optional[asyncio.Task] = None
        self._loop_ready = threading.Event()

        # Queue of asynchronously-received frames from /events. Drained
        # by run() (and ignored otherwise -- mid-turn IO_OUT bytes are
        # only meaningful while a turn is in flight, but we don't care
        # to discriminate; idle IO_OUT just gets surfaced later).
        self._events: "queue.Queue[Frame]" = queue.Queue()

        # Set when the observe task notices the underlying connection
        # has died (RST, timeout). Sync methods convert this into
        # VmCrash on their next request.
        self._dead = False

        # Last error string from a failed info() call, surfaced via
        # the diagnostics property for the driver to render.
        self._info_error: Optional[str] = None

    # ---- lifecycle ---------------------------------------------------

    def start(self) -> None:
        if self._loop_thread is not None:
            return
        self._loop = asyncio.new_event_loop()
        self._loop_thread = threading.Thread(
            target=self._run_loop, name=f"{self.name}-coap", daemon=True)
        self._loop_thread.start()
        # Wait for the worker thread to install its loop and create the
        # CoAP context before returning, so callers can immediately
        # call info()/apply_delta()/etc.
        self._loop_ready.wait(timeout=self.request_timeout)
        if not self._loop_ready.is_set():
            raise VmCrash(f"{self.name}: failed to start CoAP context")

        # The CoAP-side VM persists across driver connections (a Contiki-NG
        # device is long-lived; only the host-side driver process comes and
        # goes). The compiler in this driver process starts at watermarks
        # zero, so the device must too -- otherwise the first APPLY would
        # fail with a start_id mismatch. Issue a RESET on connect to put
        # the VM into a known fresh state. Failures are non-fatal here so
        # an old VM that doesn't support RESET still works.
        try:
            self.reset()
        except (VmError, VmCrash):
            pass

    def stop(self) -> None:
        if self._loop is None:
            return
        loop = self._loop
        try:
            asyncio.run_coroutine_threadsafe(
                self._async_stop(), loop).result(timeout=5.0)
        except FuturesTimeoutError:
            pass
        except Exception:
            pass
        try:
            loop.call_soon_threadsafe(loop.stop)
        except Exception:
            pass
        if self._loop_thread is not None:
            self._loop_thread.join(timeout=5.0)
        self._loop = None
        self._loop_thread = None
        self._context = None
        self._observe_task = None

    def is_alive(self) -> bool:
        return (self._loop is not None and not self._dead and
                self._loop_thread is not None and self._loop_thread.is_alive())

    # ---- request methods ---------------------------------------------

    def info(self) -> Optional[VmInfo]:
        # The handshake is purely informational; a failure here must
        # not poison subsequent requests. We swallow VmCrash too (which
        # _post_frame raises on timeout/transport error) and return
        # None so the banner just shows "version unknown".
        try:
            response = self._post_frame(Frame(FrameType.INFO, b""),
                                        mark_dead_on_failure=False)
        except (VmError, VmCrash) as e:
            self._info_error = str(e)
            return None
        if response is None:
            return None
        if response.type == FrameType.INFO_REPLY:
            try:
                return decode_info_reply(response.payload)
            except IOError:
                return None
        return None

    def apply_delta(self, delta_bytes: bytes) -> int:
        response = self._post_frame(Frame(FrameType.APPLY, delta_bytes))
        if response is None:
            raise VmCrash(f"{self.name}: empty APPLY response")
        if response.type == FrameType.ACK:
            if len(response.payload) < 2:
                raise VmCrash(f"{self.name}: malformed ACK")
            (start_id,) = struct.unpack("!H", response.payload[:2])
            return start_id
        if response.type == FrameType.ERROR:
            raise VmError(_decode_error(response.payload))
        raise VmCrash(
            f"{self.name}: unexpected response to APPLY: {response.type.name}")

    def run(
        self,
        entry_expr_id: int,
        *,
        on_io: Optional[IoCallback] = None,
        on_status: Optional[StatusCallback] = None,
    ) -> RunOutcome:
        # Drain any leftover events from a previous turn so the
        # terminal frame we wait for is unambiguously this turn's.
        self._drain_events()

        # POST RUN; immediate sync ACK.
        response = self._post_frame(
            Frame(FrameType.RUN, struct.pack("!H", entry_expr_id)))
        if response is None:
            raise VmCrash(f"{self.name}: empty RUN response")
        if response.type == FrameType.ERROR:
            error_type, msg = _decode_error_full(response.payload)
            return RunFailure(error_type=error_type, message=msg,
                              threads_running=0)
        if response.type != FrameType.ACK:
            raise VmCrash(
                f"{self.name}: unexpected RUN response: {response.type.name}")

        # Wait for terminal event on the observe stream.
        threads_running = 0
        while True:
            try:
                frame = self._events.get(timeout=self.request_timeout)
            except queue.Empty:
                raise VmCrash(f"{self.name}: timed out waiting for RESULT")

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
                return RunSuccess(obj_encoding=frame.payload,
                                  threads_running=threads_running)
            if frame.type == FrameType.ERROR:
                error_type, msg = _decode_error_full(frame.payload)
                return RunFailure(error_type=error_type, message=msg,
                                  threads_running=threads_running)
            # Unknown frame: surface as crash; the protocol shouldn't
            # produce anything else mid-turn.
            raise VmCrash(
                f"{self.name}: unexpected event during RUN: {frame.type.name}")

    def reset(self) -> None:
        response = self._post_frame(Frame(FrameType.RESET, b""))
        if response is None:
            raise VmCrash(f"{self.name}: empty RESET response")
        if response.type == FrameType.ACK:
            self._drain_events()
            return
        if response.type == FrameType.ERROR:
            raise VmError(_decode_error(response.payload))
        raise VmCrash(
            f"{self.name}: unexpected RESET response: {response.type.name}")

    def kill_thread(self, thread_id: int) -> None:
        self._post_frame(Frame(FrameType.KILL, struct.pack("!H", thread_id)))

    def send_io_input(self, data: bytes) -> None:
        # The device-side frontend acks IO_IN but doesn't dispatch the
        # bytes to a (read); sending them is a no-op end-to-end.
        self._post_frame(Frame(FrameType.IO_IN, data))

    # ---- internals ---------------------------------------------------

    def _post_frame(self, frame: Frame, *,
                    mark_dead_on_failure: bool = True) -> Optional[Frame]:
        if not self.is_alive():
            raise VmCrash(f"{self.name}: not running")
        coro = self._async_post(_encode_frame(frame))
        try:
            response_payload = asyncio.run_coroutine_threadsafe(
                coro, self._loop).result(timeout=self.request_timeout)
        except FuturesTimeoutError as e:
            if mark_dead_on_failure:
                self._dead = True
            raise VmCrash(f"{self.name}: request timed out") from e
        except Exception as e:
            if mark_dead_on_failure:
                self._dead = True
            raise VmCrash(f"{self.name}: request failed: {e}") from e
        return _parse_frame(response_payload)

    def _drain_events(self) -> None:
        try:
            while True:
                self._events.get_nowait()
        except queue.Empty:
            pass

    # ---- worker thread -----------------------------------------------

    def _run_loop(self) -> None:
        assert self._loop is not None
        asyncio.set_event_loop(self._loop)
        try:
            self._loop.run_until_complete(self._async_start())
        except Exception as e:
            self._dead = True
            self._loop_ready.set()
            return
        self._loop_ready.set()
        try:
            self._loop.run_forever()
        finally:
            try:
                pending = asyncio.all_tasks(loop=self._loop)
                for t in pending:
                    t.cancel()
                self._loop.run_until_complete(
                    asyncio.gather(*pending, return_exceptions=True))
            except Exception:
                pass
            self._loop.close()

    async def _async_start(self) -> None:
        self._context = await Context.create_client_context()
        self._observe_task = self._loop.create_task(self._observe_loop())

    async def _async_stop(self) -> None:
        if self._observe_task is not None:
            self._observe_task.cancel()
            try:
                await self._observe_task
            except (asyncio.CancelledError, Exception):
                pass
        if self._context is not None:
            try:
                await self._context.shutdown()
            except Exception:
                pass

    async def _async_post(self, payload: bytes) -> bytes:
        assert self._context is not None
        request = Message(code=POST, uri=self.cmd_uri, payload=payload)
        response = await self._context.request(request).response
        return bytes(response.payload)

    async def _observe_loop(self) -> None:
        """Long-running task that drives the /events observation. Each
        notification's payload carries one async frame."""
        assert self._context is not None
        request = Message(code=GET, uri=self.events_uri, observe=0)
        protocol_request = self._context.request(request)
        try:
            response = await protocol_request.response
            self._dispatch_observation(bytes(response.payload))
            async for notification in protocol_request.observation:
                self._dispatch_observation(bytes(notification.payload))
        except asyncio.CancelledError:
            raise
        except Exception:
            self._dead = True

    def _dispatch_observation(self, payload: bytes) -> None:
        if not payload:
            return
        try:
            frames = _parse_frames(payload)
        except VmCrash:
            self._dead = True
            return
        for frame in frames:
            self._events.put(frame)


def _decode_error(payload: bytes) -> str:
    _, msg = _decode_error_full(payload)
    return msg


def _decode_error_full(payload: bytes) -> tuple[int, str]:
    if len(payload) < 1:
        return (0, "")
    error_type = payload[0]
    msg = payload[1:].decode("utf-8", errors="replace")
    return (error_type, msg)
