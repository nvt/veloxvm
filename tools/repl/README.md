# velox-repl

Interactive REPL for VeloxVM. Drives a long-running compiler service
(Racket Scheme today, PyVelox for Python) and a long-running VeloxVM
REPL service over a transport-agnostic frame protocol. State persists
across forms — definitions, classes, instance attributes, and
top-level bindings stay live until you exit or `:reset`.

The driver is the only thing that talks to the user. Compiler and VM
are both subprocesses (or the VM may be a Contiki-NG device reached
over CoAP).

---

## Quick start

You need:

- [`uv`](https://docs.astral.sh/uv/) on PATH (or a Python `.venv` the
  wrapper can fall back to).
- For Scheme: `racket` on PATH.
- For Python: nothing extra — `pyvelox` is in-tree and runs under the
  same Python interpreter as the REPL itself.
- A built `bin/vm-repl` (POSIX VM with REPL support):

  ```
  make vm-repl VM_PORT=posix
  ```

Then, from `tools/repl/`:

```
./velox-repl                       # Scheme (default)
./velox-repl --language python     # Python
```

You'll see a banner reporting the auto-detected compiler and VM, then
a `velox> ` (or `pyvelox> `) prompt. Ctrl-D to exit, Ctrl-C to clear
an in-progress multi-line buffer.

```
velox> (define square (lambda (x) (* x x)))
; defined square
velox> (square 7)
49

pyvelox> def square(x): return x * x
; defined square
pyvelox> square(7)
49
```

---

## Multi-line input

The prompt switches to `... ` while the form you're typing is still
incomplete, then back when it's ready to evaluate.

**Scheme** terminates on matching parens; you can split a form across
lines naturally:

```
velox> (define fact
...      (lambda (n)
...        (if (<= n 1) 1
...            (* n (fact (- n 1))))))
; defined fact
velox> (fact 5)
120
```

**Python** uses CPython's interactive convention: indented blocks
(`def`, `class`, `if`, `for`, `while`, `try`, ...) stay open until you
**press Enter on a blank line**:

```
pyvelox> def fact(n):
...          if n <= 1:
...              return 1
...          return n * fact(n - 1)
...                                ← blank Enter terminates the def
; defined fact
pyvelox> fact(5)
120
```

One-line bodies (`def f(): return 1`) need no blank line; the empty
line is only required to terminate multi-statement indented blocks.

---

## Meta-commands

Anything starting with `:` at the prompt is a driver command, not
source.

| Command          | Effect                                                      |
|------------------|-------------------------------------------------------------|
| `:help`          | List the meta-commands.                                     |
| `:quit`, `:q`, `:exit` | Exit. Same as Ctrl-D.                                 |
| `:reset`         | Reset compiler and VM session state (forgets all definitions). |
| `:threads`       | Show running thread count on the VM.                        |
| `:sync`          | Print sync invariants (debug aid).                          |
| `:version`       | Show backend version info from the INFO_REPLY.              |

`:reset` is the recovery hatch when the driver reports `; session
desynced`. Both sides start fresh and the next form goes through
cleanly.

---

## Connecting to a Contiki-NG device

Build a Contiki-NG firmware with REPL support. There's no baked-in
program — the device exposes `/repl/cmd` and `/repl/events` CoAP
resources, and the host-side driver speaks to them.

For the **native target** (a hosted Contiki-NG process talking to the
host kernel through tun0):

```
cd ports/contiki-ng
make TARGET=native VM_REPL=1
sudo ./build/native/vm.native fd00::1/64
```

The argument `fd00::1/64` is the **host side** of the tun0 tunnel. The
process gets its own IPv6 address derived from its (default) MAC
address — typically `fd00::302:304:506:708`. The firmware logs it on
startup as `Added global IPv6 address ...`.

Sanity-check the resources:

```
aiocoap-client coap://[fd00::302:304:506:708]/.well-known/core
```

should list `</repl/cmd>` and `</repl/events>`. Then drive the REPL:

```
./tools/repl/velox-repl --vm "coap://[fd00::302:304:506:708]/repl"
```

The same protocol that runs over stdio runs over CoAP, with events
streamed via CoAP Observe (RFC 7641). Block-wise transfer (RFC 7959)
kicks in automatically for deltas larger than the link MTU.

For an embedded target, swap `TARGET=native` for the right target
(`zoul`, etc.), flash with `make ... vm.upload`, and use the address
the firmware actually announces at boot.

---

## Where the auto-detect looks

`--compiler` and `--vm` accept four shapes each:

- **omitted** — auto-detect; this is the normal mode.
- `stub` — use the in-tree test fixture (see *Test fixtures* below).
- a shell command — split with `shlex` and spawned as a subprocess.
- (`--vm` only) a `coap://[host]/repl` URI for the CoAP transport.

Auto-detect resolves, relative to the repo root:

| What       | Scheme                                              | Python                                       |
|------------|-----------------------------------------------------|----------------------------------------------|
| Compiler   | `racket languages/scheme-racket/repl-server.rkt`    | `languages/python/pyvelox-repl-server`       |
| VM         | `bin/vm-repl`                                       | (same)                                       |

Override examples:

```
./velox-repl --compiler "racket /path/to/custom-server.rkt"
./velox-repl --vm /tmp/my-vm-build
./velox-repl --vm coap://[fd00::1]/repl
```

---

## Test fixtures

Driver-side smoke testing without needing a real compiler or VM built:

```
./velox-repl --compiler stub --vm stub
```

The fixtures live in `tests/fixtures/`:

- `stub_compiler.py` — accepts a tiny Scheme-ish source dialect, emits
  real-format deltas. Useful for exercising the driver's sync
  invariants and the wire format end-to-end.
- `stub_vm.py` — accepts those deltas, returns synthetic results.
- `stub_delta.py` — shared utilities for the two stubs.

Either side can be stubbed independently — the protocol is the same
on the wire either way.

---

## Recovering from errors

| What you see                          | What it means                                                                        |
|---------------------------------------|--------------------------------------------------------------------------------------|
| `; compile error: ...`                | The compiler rejected the form. Session is intact; just retype.                      |
| `; runtime error [...]: ...`          | The VM raised an error during evaluation. The REPL main thread is auto-recovered.    |
| `; session desynced: ...`             | Compiler and VM disagree on table watermarks. Run `:reset`.                          |
| Prompt stuck at `... `                | Something is incomplete. Ctrl-C clears the buffer.                                   |
| `info handshake failed: ...`          | Banner reported the version unknown. The session is still usable; capabilities just aren't surfaced. |

---

## Troubleshooting

**Banner shows "version unknown"** — the VM didn't respond to the INFO
frame, usually because it predates the handshake. Functionality still
works; only the version line is missing.

**CoAP: `info handshake failed: ResponseTimedOut`** — device isn't
reachable. Check `ping6` to the device's address first.

**CoAP: replies stop after one form on the embedded target** —
oversized RESULT frames get encoded as `OPAQUE "result too large: NN
bytes, max MM"` rather than dropped. If you see that, simplify the
form's output or raise `COAP_MAX_CHUNK_SIZE` in the firmware's
`project-conf.h`.

**Python: `Variable 'X' conflicts with VM primitive, renamed to
'py_X'`** — pyvelox emits this on stderr the first time it remaps a
name (e.g. `add` collides with the `add` primitive). It's a heads-up,
not an error; the binding still works under the renamed identifier
internally.

**Scheme: very first form on a fresh CoAP backend hangs briefly** —
the driver issues a RESET on connect to make sure the device starts
the session fresh. That round-trip can take a few hundred ms.

---

## Layout

```
velox_repl/
  cli.py            argparse + TTY entry point
  core.py           session state machine (sync invariants, evaluate/reset)
  compiler.py       compiler subprocess client (s-expression protocol)
  vm.py             VM stdio transport + binary frame codec
  coap_transport.py CoAP transport for Contiki-NG devices (aiocoap)
  protocol.py       frame codec + s-expr helpers + INFO_REPLY decoder
  render/
    decode.py       wire-format VObj decoder
    scheme.py       VObj -> Scheme surface syntax
    python.py       VObj -> Python surface syntax
tests/
  fixtures/
    stub_compiler.py
    stub_vm.py
    stub_delta.py
  test_coap_loopback.py
```

Protocol-level details (frame layout, delta wire format, append-loader
semantics, INFO_REPLY fields) live in `doc/repl-design.md`.
