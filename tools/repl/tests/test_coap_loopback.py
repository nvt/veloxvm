"""Loopback smoke test for CoapVmClient.

Spawns a minimal aiocoap server that implements just enough of the
REPL protocol to satisfy an INFO handshake and returns echo-style
responses for APPLY/RESET/RUN. Confirms that CoapVmClient's threading
and frame parsing work end-to-end without needing a real device.

Run via:  uv run --quiet python tools/repl/tests/test_coap_loopback.py
"""

from __future__ import annotations

import asyncio
import struct
import threading
import time

from aiocoap import CHANGED, CONTENT, Context, Message, resource

from velox_repl.coap_transport import CoapVmClient
from velox_repl.protocol import (
    CAP_IO_OUT,
    Frame,
    FrameType,
    PROTOCOL_VERSION,
    VmInfo,
    encode_info_reply,
)


# ---------------------------------------------------------------------------
# Fake CoAP server
# ---------------------------------------------------------------------------


class CmdResource(resource.Resource):
    """POST /repl/cmd handler. Builds a reply frame for each command."""

    def __init__(self, events_resource: "EventsResource"):
        super().__init__()
        self._events = events_resource

    async def render_post(self, request):
        payload = bytes(request.payload)
        if len(payload) < 3:
            return Message(code=CHANGED,
                           payload=_frame(FrameType.ERROR, b"\x02bad"))
        ftype = payload[0]
        body = payload[3:]

        if ftype == FrameType.INFO:
            info = VmInfo(
                protocol_version=PROTOCOL_VERSION,
                bytecode_version=3,
                capabilities=CAP_IO_OUT,
                name="loopback-vm",
                version="0.1",
                build="loopback",
            )
            return Message(code=CHANGED,
                           payload=_frame(FrameType.INFO_REPLY,
                                          encode_info_reply(info)))
        if ftype == FrameType.APPLY:
            # ACK with start_id 1 regardless of delta contents.
            return Message(code=CHANGED,
                           payload=_frame(FrameType.ACK,
                                          struct.pack("!H", 1)))
        if ftype == FrameType.RUN:
            # Schedule an IO_OUT then a RESULT frame as observe events.
            self._events.queue_async(
                _frame(FrameType.IO_OUT, b"hello, "),
                _frame(FrameType.IO_OUT, b"world"),
                _frame(FrameType.RESULT, bytes([0x01])),  # NONE
            )
            return Message(code=CHANGED,
                           payload=_frame(FrameType.ACK,
                                          struct.pack("!H", 1)))
        if ftype == FrameType.RESET:
            return Message(code=CHANGED,
                           payload=_frame(FrameType.ACK, b"\x00\x00"))
        return Message(code=CHANGED,
                       payload=_frame(FrameType.ERROR, b"\x00unknown"))


class EventsResource(resource.ObservableResource):
    """GET /repl/events handler. Pops one queued event per render_get
    call. update_observation_count drives notifications."""

    def __init__(self):
        super().__init__()
        self._queue: list[bytes] = []

    def queue_async(self, *frames: bytes):
        # Called from the request handler context; safe because both
        # run on the same asyncio loop. Trigger one notification; the
        # GET handler packs the entire queue into the body since
        # observe is lossy.
        self._queue.extend(frames)
        self.updated_state()

    async def render_get(self, request):
        # Drain the whole queue into one notification body.
        payload = b"".join(self._queue)
        self._queue.clear()
        return Message(code=CONTENT, payload=payload)


def _frame(ftype: FrameType, body: bytes) -> bytes:
    return bytes([int(ftype)]) + struct.pack("!H", len(body)) + body


# ---------------------------------------------------------------------------
# Server lifecycle
# ---------------------------------------------------------------------------


SERVER_PORT = 56830  # high port to avoid root requirement


async def _serve(ready: threading.Event, stop: threading.Event):
    root = resource.Site()
    events = EventsResource()
    cmd = CmdResource(events)
    root.add_resource(("repl", "cmd"), cmd)
    root.add_resource(("repl", "events"), events)
    ctx = await Context.create_server_context(
        root, bind=("127.0.0.1", SERVER_PORT))
    ready.set()
    while not stop.is_set():
        await asyncio.sleep(0.1)
    await ctx.shutdown()


def _run_server(ready: threading.Event, stop: threading.Event):
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    loop.run_until_complete(_serve(ready, stop))
    loop.close()


# ---------------------------------------------------------------------------
# Test
# ---------------------------------------------------------------------------


def main() -> int:
    ready = threading.Event()
    stop = threading.Event()

    server_thread = threading.Thread(
        target=_run_server, args=(ready, stop), daemon=True)
    server_thread.start()
    if not ready.wait(timeout=5.0):
        print("FAIL: server didn't start")
        return 1

    base = f"coap://127.0.0.1:{SERVER_PORT}/repl"
    print(f"connecting to {base}")

    client = CoapVmClient(base, name="loop", request_timeout=10.0)
    client.start()
    print("start OK")
    try:
        info = client.info()
        print(f"info: {info}")
        if info is None:
            print("FAIL: no info received")
            return 1

        ack = client.apply_delta(b"\x5e\xb6\x01")
        print(f"apply ack start_id: {ack}")

        outputs: list[bytes] = []
        outcome = client.run(1, on_io=lambda b: outputs.append(b))
        print(f"run outcome: {outcome}")
        print(f"io_out captured: {b''.join(outputs)!r}")

        client.reset()
        print("reset OK")

        if info.name != "loopback-vm":
            print(f"FAIL: unexpected info name {info.name!r}")
            return 1
        if b"".join(outputs) != b"hello, world":
            print(f"FAIL: unexpected IO_OUT bytes")
            return 1
        print("PASS")
        return 0
    finally:
        try:
            client.stop()
        except Exception as e:
            print(f"stop error: {e}")
        stop.set()
        server_thread.join(timeout=5.0)


if __name__ == "__main__":
    raise SystemExit(main())
