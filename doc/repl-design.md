# VeloxVM Interactive REPL — Design Document

## Overview

This document specifies the design of an interactive Read-Eval-Print
Loop (REPL) for VeloxVM. The REPL allows a developer to evaluate
Scheme (and, later, Python or Cyclus) expressions against a live VM,
with definitions and other state persisting across inputs. The same
design supports remote REPL sessions to a Contiki-NG node over UDP/CoAP.

The implementation is split across three components with deliberately
sharp boundaries: a stateful **compiler service**, a stateful **VM REPL
service**, and a thin **REPL driver** that brokers between them and
talks to the user (or to an editor). The boundaries are chosen so that
no component depends on the surface language of any other: the VM
doesn't know what produced its bytecode, the compiler doesn't know what
runs its output, and the driver doesn't know what's being evaluated.

## Goals and Non-Goals

### Goals

- A state-preserving REPL: `define` bindings, mutated globals, loaded
  libraries, and live closures persist across REPL inputs.
- Zero overhead on production embedded deployments. All REPL code is
  gated behind `VM_REPL_ENABLE` and is omitted from default builds for
  Contiki-NG and other resource-constrained ports.
- Multiple surface languages on the same VM (Scheme via Racket compiler
  today; PyVelox and Cyclus added by introducing new compiler services
  with no driver/VM changes).
- A single wire protocol usable over any transport: stdio (subprocess),
  Unix domain socket (local daemon), UDP (LAN), CoAP (constrained IoT
  devices).
- Support for editor and IDE integration (Emacs, Neovim, VS Code,
  Jupyter) by exposing the driver's API as JSON-RPC.

### Non-Goals (v1)

- Sharing a single REPL session across multiple surface languages.
  Each session is single-language; switching languages requires `:reset`.
- Synchronous application input (`(read)`) on embedded targets. Routed
  through frames where the device is connected to a host driver, but
  not implemented in the v1 stub.
- Persistent REPL sessions across VM restarts. Each session lives only
  as long as the underlying VM process.
- Performance parity with batch compilation. Interactive turns are
  expected in the tens-of-milliseconds range on POSIX; the
  optimizations that batch compilation enables (whole-program inlining,
  cross-form analysis) are deferred.

## Architecture

```
   ┌──────────────┐    stdio     ┌──────────────┐   stdio | unix sock |
   │   compiler   │◄────────────►│              │   UDP-CoAP | serial
   │   service    │              │ REPL driver  │◄──────────────────────┐
   │ (long-run)   │              │ (broker+UI)  │                       │
   └──────────────┘              └──────────────┘                       ▼
                                       ▲                       ┌────────────────┐
                                       │                       │  VeloxVM REPL  │
                                       ▼                       │    service     │
                                  user TTY                     │ (POSIX/Contiki │
                                  (or editor)                  │      -NG)      │
                                                               └────────────────┘
```

The driver is the only component that talks to the user. The two
services are pure stateful evaluators of opaque messages.

### Component responsibilities

| Component                | Owns                                            | Must not know                                |
|--------------------------|-------------------------------------------------|----------------------------------------------|
| Compiler service         | source → bytecode delta; macro/expander state   | the transport, the VM, or the user           |
| VM REPL service          | apply delta → run → emit result and I/O frames  | the source language or the compiler          |
| REPL driver              | UX, transport selection, sync invariants        | Scheme/Python semantics or VM internals      |

This decoupling is what enables the IoT story (the VM service runs on a
constrained device while the compiler stays on the host) and the
multi-language story (drop in PyVelox compiler service, no other code
changes).

## Components

### Compiler service

A long-running subprocess that exposes one operation: "compile this
source into a delta." For Scheme this is `racket
languages/scheme-racket/repl-server.rkt`, which wraps the existing
`compile-string` machinery in a session that holds the bytecode object
and macro environment across calls. PyVelox would add an equivalent
`pyvelox-repl-server`.

State held across calls:

- **Bytecode object** with monotonically growing string, symbol,
  expression, and captures tables. New items get IDs starting at the
  current end of each table.
- **Macro/expander environment**: `define-syntax` bindings,
  scope/mark state, phase-1 bindings.
- **Sync watermarks**: `sent-strings`, `sent-symbols`, `sent-exprs` —
  the index up to which the VM has acknowledged. Each delta covers
  items from the watermark to the current table size.
- **Optional source-location map** for error reporting on past forms.

State *not* held: source text of past forms, runtime values, transport
or VM connection details.

A failed compile must not mutate session state. Two acceptable
strategies: snapshot before each call and restore on exception, or
stage all mutations into a scratch object and commit only on success.
The expander's hidden mutable state (scope counters, hash tables) must
participate in the chosen strategy or session integrity is lost.

### VM REPL service

A long-running entity (POSIX subprocess, daemon, or device firmware
component) that holds a single live `vm_program_t` — the **REPL
program** — and applies deltas into it. State held:

- The REPL `vm_program_t` with its growing strings/symbols/exprs/
  captures tables and `symbol_bindings` array.
- The program's **main thread**, created once at program creation and
  reused across every REPL turn (see "Thread Model" below).
- All additional threads spawned by user-level `(thread-create!)`
  calls in previously evaluated forms that haven't yet exited.
- An expected-next-`start_id` watermark per table, used to detect and
  discard duplicate deltas (idempotency under retransmission).

The service is C code in `core/vm-repl-service.c` (transport-agnostic)
plus a thin transport frontend in each port directory. All gated by
`VM_REPL_ENABLE`. See "Append-loader" below for the table-growth
mechanics and "Thread Model" for how a single thread is reused across
every turn.

### REPL driver

The driver is the only stateful component the user-facing process needs
to be aware of. State held:

- Connection handles for the compiler and the VM (process, socket fds,
  CoAP client context).
- `last_acked_start_id` and `in_sync` — the cross-peer sync invariants.
- Multi-line input buffer (when the compiler returned `(incomplete)`).
- Per-turn transient state (current entry expr_id, IO_OUT accumulator).
- Configuration: language choice, transport, history file, timeouts.

State *not* held: bytecode, symbol tables, defined names, or any other
mirror of compiler/VM state. The driver caches nothing about evaluation.

The reference implementation is Python under `tools/repl/`, managed
with `uv` and exposing both a TTY frontend and (in a future revision)
JSON-RPC for editor integration.

## Driver–Compiler Protocol

S-expressions, line-framed, on the compiler subprocess's stdin/stdout.
One request per call; one response per request. Both ends use Unicode
UTF-8.

### Requests (driver → compiler)

```
(compile-form "<source>")
(reset)
(shutdown)
```

`compile-form` takes a Scheme/Python/etc. source string and asks the
compiler to compile it as a top-level form against the current session.
`reset` discards the entire session state (bytecode object, expander
env, sync watermarks). `shutdown` causes the compiler to exit; no
response is sent.

### Responses (compiler → driver)

```
(ok "<delta-base64>" <entry-expr-id> <kind> [<name>])
(incomplete)
(error "<msg>" (<line> <col>))
```

`<kind>` is one of:

- `expr` — an expression form whose runtime value should be auto-printed
  by the driver after RUN. The compiler is responsible for emitting
  the wrapper bytecode that places the value on the IO_OUT stream
  (e.g., wrapping the form in `(write x) (newline)`).
- `stmt` — a statement that yields no displayable value
  (`define-syntax`, top-level imports, Python control-flow statements).
  The driver should not print a value.
- `define` — a top-level binding. The optional `<name>` is the bound
  symbol; the driver typically prints `; defined <name>`.

`<delta-base64>` is the wire-format delta (see "Delta Wire Format"
below) base64-encoded for transit through the s-expression protocol.

`(incomplete)` indicates the source is not yet a complete top-level
form — the driver should accept more user input and re-send the
accumulated buffer. Whether a form is complete is determined by the
*compiler*, not the driver, because the rules differ across surface
languages (paren balance for Scheme, indentation/colons for Python).

`(error "<msg>" (<line> <col>))` is a recoverable compile-time error.
The session state is *unchanged* by a request that returns an error;
the driver may retry with corrected source.

### Sequencing

Strictly request/response, no overlap. The compiler does not emit
unsolicited messages. There is no "interrupt" message — `shutdown`
followed by spawning a fresh process is the only way to abort a
runaway compile.

## Driver–VM Protocol

Binary frames, transport-agnostic. The same bytes flow over stdio,
Unix sockets, UDP, or CoAP block-wise transfer.

### Frame format

```
Offset  Size  Description
------  ----  -----------
0x00    1     Frame type (uint8)
0x01    2     Payload length (uint16, network byte order)
0x03    L     Payload (length bytes)
```

Maximum payload size is 65535 bytes (uint16). Larger deltas must be
split into multiple APPLY frames or carried via a transport-level
fragmentation mechanism (CoAP block-wise).

### Frame types

| Tag    | Name      | Direction       | Payload                                      |
|--------|-----------|-----------------|----------------------------------------------|
| 0x01   | APPLY     | driver → VM     | delta bytes (see "Delta Wire Format")        |
| 0x02   | RUN       | driver → VM     | entry expr_id (uint16)                       |
| 0x03   | RESET     | driver → VM     | empty                                        |
| 0x04   | KILL      | driver → VM     | thread_id (uint16)                           |
| 0x05   | IO_IN     | driver → VM     | bytes for application `(read)` (reserved)    |
| 0x10   | RESULT    | driver ← VM     | obj encoding (see "Runtime Value Encoding")  |
| 0x11   | ERROR     | driver ← VM     | error_type (uint8) + UTF-8 message           |
| 0x12   | IO_OUT    | driver ← VM     | bytes from `display` / `write`               |
| 0x13   | STATUS    | driver ← VM     | thread_count (uint16) + flags (uint16)       |
| 0x14   | ACK       | driver ← VM     | start_id of last-applied delta (uint16)      |

### Sequencing

A typical turn:

```
driver -> APPLY(delta_bytes)
driver <- ACK(start_id)
driver -> RUN(entry_expr_id)
driver <- IO_OUT(bytes...)         (zero or more, async)
driver <- STATUS(threads_running)  (optional, may be repeated)
driver <- RESULT(obj)              -- or --   ERROR(type, msg)
```

`IO_OUT` and `STATUS` frames may interleave during evaluation. `RESULT`
or `ERROR` ends the turn; either implies the main thread parked (or
errored). Background threads spawned by the form via user-level
`(thread-create!)` remain running and may emit `IO_OUT` between turns
(driver-side rendering: see "Application I/O").

`APPLY` is idempotent: the VM compares the delta's `start_id` against
its expected-next watermark and silently discards duplicates. This
makes the protocol robust over UDP without stateful retransmission
logic in the driver.

`RESET` discards the REPL program; ACK confirms. `KILL` requests
termination of the named thread; the VM may emit additional `STATUS`
frames after the kill takes effect.

### Error semantics

`ERROR` payload format:

```
Offset  Size  Description
------  ----  -----------
0x00    1     Error type (matches vm_error_type_t in vm.h)
0x01    var   UTF-8 message
```

Errors during APPLY are unusual (delta version mismatch, OOM in the
table-grow path) and indicate that the session is desynced; the driver
should reset both peers.

Errors during RUN are normal (unbound symbol, division by zero,
unhandled exception). They terminate the turn but leave session state
intact — any side effects the form had before the error (e.g., a
`(define)` evaluated before a later sub-expression threw) are
preserved.

## Delta Wire Format

A delta describes append operations against the VM's REPL program. It
shares a magic prefix with the existing whole-program bytecode format
but is not a complete program — only the new items.

```
Offset  Size  Description
------  ----  -----------
0x00    1     File ID 1: 0x5E
0x01    1     File ID 2: 0xB6   (compare 0xB5 for full programs)
0x02    1     Bytecode version (currently 1)
0x03    var   TLV sections (in any order; see below)
```

The differing magic byte (`0xB6` vs `0xB5`) lets the loader and the
`file(1)` utility distinguish a delta from a full program at a glance,
and prevents a delta from being mistakenly loaded by `vm_load_program`.

### Sections (TLV-encoded)

Each section is:

```
Offset  Size  Description
------  ----  -----------
0x00    1     Section tag
0x01    2     Section length (uint16, network byte order)
0x03    L     Section payload
```

| Tag    | Name              | Payload                                                          |
|--------|-------------------|------------------------------------------------------------------|
| 0x01   | STRINGS_APPEND    | start_id (u16) + count (u16) + count × (length-prefixed bytes)   |
| 0x02   | SYMBOLS_APPEND    | start_id (u16) + count (u16) + count × (length-prefixed bytes)   |
| 0x03   | EXPRS_APPEND      | start_id (u16) + count (u16) + count × (length-prefixed bytes)   |
| 0x04   | CAPTURES_APPEND   | count (u16) + count × (length-prefixed entry, see below)         |
| 0x05   | ENTRY_EXPR        | expr_id (u16) — which expression to RUN                          |
| 0xFF   | END               | empty                                                            |

Each item in STRINGS / SYMBOLS / EXPRS is encoded the same way as in
the existing whole-program format: 2-byte length followed by raw bytes.

Each entry in CAPTURES_APPEND is:

```
entry_length:u16 expr_id:u16 sym_id_0:u16 sym_id_1:u16 ...
```

`start_id` in the append sections is redundant (must equal the
receiver's expected next index) but the VM uses it to detect duplicate
deltas and reject out-of-order ones.

A delta need not include every section type. A delta that defines a
simple integer expression like `(+ 1 2)` typically contains only an
EXPRS_APPEND with one item plus an ENTRY_EXPR.

### Maximum delta size

Limited by the surrounding APPLY frame's 16-bit length field (65535
bytes). Larger deltas must be split. In practice, a single REPL form
rarely exceeds a few hundred bytes of bytecode; the limit only matters
for unusual inputs (large quoted literals, big constant tables) and
for the CoAP path where 6LoWPAN MTUs require block-wise fragmentation.

## Runtime Value Encoding

The VM serializes its tagged `vm_obj_t` values into a stable wire
encoding inside `RESULT` frames. The driver-side decoder is independent
of the VM's internal C structs.

```
Tag     Name        Payload
------  ----------  -------
0x01    NONE        (no payload)
0x02    BOOLEAN     1 byte (0 or 1)
0x03    INTEGER     8 bytes signed, network byte order
0x04    REAL        8 bytes IEEE 754, network byte order
0x05    CHAR        4 bytes Unicode codepoint
0x06    STRING      2-byte length + UTF-8 bytes
0x07    SYMBOL      2-byte length + UTF-8 bytes
0x08    LIST        2-byte count + count × sub-encoding
0x09    VECTOR      2-byte count + count × sub-encoding
0x0A    PAIR        car sub-encoding + cdr sub-encoding (improper lists)
0xF0    OPAQUE      2-byte length + UTF-8 descriptor (closures, ports, ...)
```

The driver renders these per-language: Scheme produces `(1 2 3)`,
`#t`, `#\a`, `"hello"`; Python produces `[1, 2, 3]`, `True`, `'a'`,
`'hello'`. Same VM bytes, different surface syntax.

## State Model

### What each component holds

```
Compiler service                  VM REPL service              REPL driver
-----------------                 -------------------          -----------
bytecode object                   vm_program_t                 connection handles
  strings table                     strings table              last_acked_start_id
  symbols table                     symbols table              in_sync flag
  exprs table                       exprs table                input buffer
  captures table                    captures table             current_turn (transient)
expander env                        symbol_bindings array      config (immutable)
sync watermarks                     captures array
                                    live threads
                                    expected-next watermarks
```

The compiler's and VM's tables are *parallel mirrors* of the same
incrementally built program. Strings and symbols are interned in the
compiler and stored by ID in the VM. Expressions are bytecode-encoded
in the compiler and stored verbatim in the VM. The driver's only job
is keeping their watermarks aligned.

### What no one holds

- Source text of past forms. Once compiled, the compiler forgets.
  If the driver implements `:replay`, it stores source strings.
- Macro expansion of past forms. Cached by the compiler implicitly via
  the bytecode object; never reified.
- Runtime values across turns. They live in `vm_program_t.symbol_bindings`
  (for `define`d names) or in the heap reachable from there. The driver
  sees only RESULT-frame encodings, never internal `vm_obj_t`s.

## Sync Invariants

Two invariants the driver enforces:

1. **Watermark monotonicity**: `last_acked_start_id` only increases. A
   delta is in flight if it's been sent to the VM but not yet ACKed.
   At most one delta is in flight at a time per session.
2. **Lockstep reset**: a `RESET` propagates first to the compiler, then
   to the VM. While `in_sync` is false, the driver refuses new
   `compile-form` calls and only accepts `:reset` (or `:quit`). After
   both peers ACK their resets, watermarks return to zero and `in_sync`
   becomes true.

Failure recovery: if either peer crashes mid-session, the driver marks
`in_sync = false` and surfaces a desync to the user. Resetting both
peers restores the invariants. There is no automatic restart of a
crashed peer in v1.

## Transports

The frame protocol is identical across all transports; only the byte
source/sink differs.

| Transport             | Use                              | Notes                                       |
|-----------------------|----------------------------------|---------------------------------------------|
| stdio (subprocess)    | local POSIX, default             | Driver `fork`+`exec`s `bin/vm-repl`         |
| Unix domain socket    | local POSIX, daemon mode         | Long-lived service; multiple drivers attach |
| UDP                   | LAN testing of IoT path          | Datagram-bounded; needs fragmentation       |
| CoAP over UDP         | Contiki-NG remote REPL           | Block-wise transfer + Observe + confirmable |

A new transport is implemented by writing a small "pump" that reads
incoming bytes, hands frames to `vm_repl_service_handle()`, and writes
outgoing frames to whatever the underlying medium is.

### CoAP resource design (Contiki-NG)

The device's CoAP server registers two resources:

- `POST /repl/cmd` — body is a protocol frame (APPLY, RUN, RESET,
  KILL). Confirmable so the driver knows it landed. Block-wise handles
  deltas larger than the link MTU.
- `OBSERVE /repl/events` — emits async frames (RESULT, ERROR, IO_OUT,
  STATUS) via CoAP Observe (RFC 7641). The driver registers once and
  receives a stream of notifications.

## Application I/O

Application output (`display`, `write`, `(newline)`, `print()`) is
emitted by the VM as `IO_OUT` frames during evaluation. The driver
prints the bytes to the user's terminal as they arrive. This works
identically whether the VM is local (stdio subprocess) or remote
(Contiki-NG over CoAP) — there's no special-case TTY sharing.

Application input (`read`, `read-line`) flows the other way over
the same channel. The protocol reserves an `IO_IN` frame: when a
`(read)` blocks, the VM is intended to emit a STATUS frame
indicating "waiting on input" and the driver forwards user-typed
bytes as `IO_IN`. The device-side frontend currently acks the
frame at protocol level and drops the bytes without dispatching
them; the host-side `(read)` plumbing isn't part of this design's
scope.

Background threads spawned by a REPL form keep running across REPL
turns. Their `IO_OUT` may interleave with the next prompt; the driver
clears the prompt line, prints the bytes, and redraws. The scheduler
runs only inside `vm_repl_service_run()`, so background work is paused
between turns unless the driver explicitly issues idle ticks (a v2
feature).

## Multi-Language Support

A new surface language is added by writing a new compiler service that
speaks the s-expression protocol. The VM REPL service, the wire
protocol, and the driver core are unchanged. The driver gains:

- A registry entry mapping language name to compiler spawn command and
  result renderer.
- A renderer module under `tools/repl/velox_repl/render/` that formats
  decoded `RESULT` frame contents in that language's surface syntax.

Each REPL session is single-language. Mixing requires a shared symbol
allocator across compilers and is deferred indefinitely. To switch
languages, the user runs `:reset` followed by `:lang <name>` (which
respawns the compiler service).

The auto-print decision (whether to wrap a form so its value is
displayed) is per-language and made by the compiler:

- Scheme: wrap unless the form is `define` / `define-syntax`.
- Python: wrap if the top-level statement is an `Expression`; don't
  wrap for `Assignment`, `def`, `class`, `import`, etc.

The compiler reports `kind` (`expr` / `stmt` / `define`) in its
response so the driver knows what to expect on the IO_OUT stream.

## Embedded REPL on Contiki-NG

The append-loader and `vm-repl-service.c` are portable C; they compile
for Contiki-NG unchanged. The CoAP frontend
(`ports/contiki-ng/vm-repl-coap.c`) registers the `/repl/cmd` and
`/repl/events` resources on the device's existing CoAP server. The
host driver connects via CoAP and ships deltas the same way it would
locally.

### Memory budget

The REPL program's tables grow with every input. On a 32 KB RAM target,
an unbounded session exhausts the heap quickly. Two guardrails:

- Configurable `VM_REPL_MAX_HEAP_BYTES` ceiling. When exceeded, the VM
  emits `ERROR resource_exceeded` and refuses further APPLYs.
- `:reset` is treated as a normal mode of operation, not exceptional.
  Embedded sessions are short-lived "inspect, tweak, reset" loops.

### Code size

With `-Os` on Contiki-NG, enabling `VM_REPL_ENABLE` adds approximately
3–5 KB of ROM (append-loader ≈400 LOC, vm-repl-service ≈300 LOC, CoAP
glue ≈150 LOC). Off by default; enabled only in development firmware
images.

### Application I/O on embedded

`(display)` / `(write)` output is emitted as `IO_OUT` frames over the
CoAP Observe stream and rendered on the host driver's terminal.
`(read)` is stubbed in v1; routing it back from the host is a v2
feature.

## Thread Model

A REPL turn does not spawn a new thread per evaluation. Instead, the
program's **main thread** — created once at `vm_repl_program_create`,
exactly as `vm_load_program` does for ordinary programs — is reused
across every turn. The right mental model is a *parked continuation*:
the thread is alive throughout the session, parked between turns, and
unparked when the next form's bytecode is ready.

### Lifecycle

The main thread cycles through three states:

```
   PARKED  ──[apply_delta + run]──▶  RUNNABLE  ──[scheduler]──▶  RUNNING
      ▲                                                              │
      │                                                              │
      └─────────[bytecode exhausted at top frame]────────────────────┘
```

- At `vm_repl_program_create`, the main thread is created in `PARKED`
  state with no entry pointer set.
- `vm_repl_run` updates the parked thread's top frame so its `ip`
  points at the new entry expression's bytecode and `end` points one
  past the last byte. It marks the thread `RUNNABLE` and runs the
  scheduler.
- When the bytecode is exhausted at the top frame, the scheduler
  parks the thread (a new `VM_THREAD_PARKED` state) instead of
  destroying it. `vm_repl_run` returns at that point and serializes
  `thread->result`.
- `vm_repl_program_destroy` is the only path that destroys the main
  thread.

### Why this is correct

At the boundary between two REPL turns, the thread's transient state
is empty by construction: the top-level form has finished evaluating,
so the frame stack is back to its bottom. Anything the form *did*
persist — `define`d names, mutated globals, captured closures — lives
in `program->symbol_bindings` and the program's heap, not in the
thread. So nothing about the thread needs to be reset between turns:
it can keep going as soon as it has more bytecode to execute. The
"reset" framing is misleading; "park and resume" is what's actually
happening.

### User-level `(thread-create!)`

Forms may call the user-level `thread-create!` primitive, which
allocates a *separate* `vm_thread_t` for the body the user passed in.
That spawning happens through the existing primitive in
`core/expr-thread.c` and is unrelated to the main thread's parked
state. Threads created this way keep running across REPL turns and
emit `IO_OUT` exactly as the main thread does.

`vm_repl_run` returns when the *main* thread parks, regardless of
whether other user-spawned threads are still running. The driver
chooses how to surface their continuing work (see "Application I/O").

### Pointer stability

Each REPL form's bytecode lives in its own freestanding `VM_MALLOC`
buffer (one per `EXPRS_APPEND` item). The thread's `ip` and `end`
hold raw pointers into that buffer. Because each form has its own
allocation, no buffer ever needs to be `realloc`'d while the thread
is mid-evaluation — `ip` and `end` remain stable for the lifetime of
the form. (The naive alternative of growing a single concatenated
expression in place would dangle the thread's pointers on every
append.)

## Append-Loader (C-side)

`vm_program_t` today holds three fixed-size tables backed by a
contiguous arena. The REPL program is the same struct, but its tables
must grow as deltas arrive. The clean approach:

- Original arena bytes (if any) stay untouched. Each appended item
  gets its own `VM_MALLOC` allocation.
- The `items[]` and `item_lengths[]` index arrays grow geometrically
  via `realloc`.
- A new field on `vm_table_t` (`arena_item_count`) marks the boundary
  between arena-resident items and freestanding items, so
  `vm_table_destroy` knows what to free.
- The interpreter's hot path (`VM_TABLE_GET`, `VM_TABLE_LENGTH`) is
  unchanged because `items[i]` remains a raw pointer regardless of
  where the item lives.
- The new field is itself behind `#ifdef VM_REPL_ENABLE`. Production
  embedded builds see byte-identical struct layout to today.

`symbol_bindings` and `captures` arrays grow in lockstep with
`symbols.item_count` and `exprv.item_count` respectively, with new
entries zero-initialized. GC marking iterates `VM_TABLE_SIZE(symbols)`
and so picks up the growth automatically; resize and `item_count`
update must occur within a single `vm_repl_apply_delta` call so GC
never sees an inconsistent state.

API surface (header in `include/vm-repl.h`, implementation in
`core/vm-repl-service.c`):

```c
vm_program_t *vm_repl_program_create(const char *name,
                                     const vm_policy_t *policy);
int vm_repl_apply_delta(vm_program_t *p,
                        const uint8_t *bytes, size_t len,
                        vm_expr_id_t *out_entry_expr_id);
int vm_repl_run(vm_program_t *p, vm_expr_id_t entry,
                vm_obj_t *out_result, vm_error_t *out_error);
void vm_repl_program_destroy(vm_program_t *p);
```

`vm_repl_program_create` allocates the program *and* its main thread
in `VM_THREAD_PARKED` state. `vm_repl_run` redirects the parked main
thread to evaluate `entry`, runs the scheduler until the thread parks
again, then serializes `thread->result` into `out_result`. It does
not spawn or destroy threads. `vm_repl_program_destroy` tears down
the main thread along with the program. The transport frontend wraps
these in a frame-pump loop.

## Frontends

The driver exposes its session API in three forms:

- **TTY** (default): prompt_toolkit-based interactive frontend with
  history, multi-line accumulation, and meta-commands. Falls back to
  stdlib readline-style input when stdin is not a terminal (so
  `velox-repl < script.scm` works for testing).
- **JSON-RPC on stdio** (planned): same operations exposed as
  request/response messages for editor and IDE integration. Methods:
  `evaluate`, `complete`, `inspect`, `reset`, `interrupt`,
  `set-language`. Notifications: `output`, `thread-status`, `error`.
- **Jupyter kernel** (future): wraps the driver core as an `ipykernel`
  subclass. Yields VS Code notebook support, JupyterLab support, and
  `jupyter console` for free.

Editor plugins (Emacs Geiser-style backend, Neovim Lua plugin, VS Code
extension) consume the JSON-RPC frontend and supply their own UX.

## Build Configuration

`VM_REPL_ENABLE` is the master switch. When undefined (default for
embedded ports), the following are excluded from the build:

- `core/vm-repl-service.c` and `core/vm-loader-append.c`
- The `arena_item_count` field on `vm_table_t`
- All transport frontends except stdio (which is also conditional)

POSIX builds enable the flag for `bin/vm-repl` while leaving `bin/vm`
unchanged. Contiki-NG enables it only in explicitly-marked dev
firmware images.

## Open Questions

- **Result of expression vs. effect of definition**. Should `define` go
  through the same auto-print machinery (printing nothing) or be
  short-circuited at the compiler level (no wrapper emitted)? Current
  design favors the latter for code-size reasons on the device.
- **GC scheduling**. Long REPL sessions accumulate heap pressure. Is
  the existing GC trigger sufficient, or should `vm_repl_apply_delta`
  invoke a sweep when watermarks cross a configurable threshold?
- **Symbol-id space**. `vm_symbol_id_t` is 16-bit. A long session with
  many gensyms can exhaust it. v1 documents the limit; v2 may need
  symbol garbage collection.
- **Cross-process compiler crash recovery**. If the compiler subprocess
  crashes mid-session, can we restart it and recover the bytecode
  object from the VM's tables, or is `:reset` the only option?
  Currently `:reset` is the only option.

## References

- `doc/bytecode-format.md` — full-program bytecode format (the delta
  format shares the magic prefix and table item encoding).
- `doc/instruction-set.md` — VM opcode reference.
- `tools/repl/` — driver implementation (Python).
- `tools/repl/README.md` — user-facing usage notes.
