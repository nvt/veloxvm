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
