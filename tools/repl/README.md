# velox-repl

Interactive REPL driver for VeloxVM. Coordinates a long-running compiler
process (Racket Scheme compiler today, PyVelox later) and a long-running
VeloxVM REPL service over a transport-agnostic frame protocol. The driver
is the only thing that talks to the user.

## Running

If you have [uv](https://docs.astral.sh/uv/):

```
./velox-repl                # uses uv's cached env automatically
```

Without uv, the wrapper falls back to a project-local `.venv`:

```
./velox-repl                # creates .venv on first run, then uses it
```

Either way: no manual `source activate`. The wrapper is in `tools/repl/`
and is intended to be symlinked from `~/.local/bin/` if you want it on
PATH globally.

## Demo

Default backends are the in-tree stubs, which let the driver be
exercised end-to-end without any other components built:

```
./velox-repl --compiler stub --vm stub
```

The real Racket compiler service can be plugged in independently of
the C-side VM (the stub VM accepts real-format deltas and returns a
synthetic placeholder result, so the protocol still round-trips):

```
./velox-repl --compiler "racket ../../languages/scheme-racket/repl-server.rkt"
```

When the C-side append-loader is built, replace `--vm stub` with the
real `bin/vm-repl` binary; the protocol on the wire is unchanged.

## Talking to a Contiki-NG device over CoAP

Build a Contiki-NG firmware with REPL support enabled (no baked-in
program, exposes `/repl/cmd` and `/repl/events` resources):

```
cd ports/contiki-ng
make TARGET=native VM_REPL=1
sudo ./build/native/vm.native fd00::1/64
```

The argument `fd00::1/64` is the **host side** of the tun0 tunnel.
The simulated device gets a separate IPv6 address derived from its
MAC address. With the default MAC `01:02:03:04:05:06:07:08`, the
device's address is `fd00::302:304:506:708` (after the EUI-64 U-bit
flip). The firmware logs it on startup as `Added global IPv6
address ...`. If you change `PLATFORM_CONF_MAC_ADDR` you'll get a
different address.

A quick sanity check via `aiocoap-client`:

```
aiocoap-client coap://[fd00::302:304:506:708]/.well-known/core
```

should list `</repl/cmd>` and `</repl/events>`. Then drive the REPL:

```
./tools/repl/velox-repl --vm "coap://[fd00::302:304:506:708]/repl"
```

The same protocol travels over CoAP that travels over stdio, with
events streamed via CoAP Observe. Block-wise transfer is supported
by aiocoap automatically when delta payloads exceed the link MTU.
For Cooja simulation or a real device, swap `TARGET=native` for the
appropriate target (`zoul`, `cooja-mote`, etc.) and use the address
the firmware actually announces at boot.

## Layout

```
velox_repl/
  cli.py          argparse + TTY entry point
  core.py         session state machine (sync invariants, evaluate/reset/interrupt)
  compiler.py     compiler subprocess client (s-expression protocol)
  vm.py           VM transport + binary frame protocol (async events)
  protocol.py     frame codec + s-expr helpers
  render/
    scheme.py     vm_obj_t -> Scheme surface syntax
    python.py     vm_obj_t -> Python surface syntax (placeholder)
  stubs/
    stub_compiler.py    fake compiler for stage 1
    stub_vm.py          fake VM for stage 1
```

See `doc/repl-design.md` (forthcoming) for the protocol spec.
