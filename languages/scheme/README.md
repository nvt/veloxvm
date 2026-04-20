# VeloxVM Common Lisp Scheme Compiler

The original Scheme-to-bytecode compiler for VeloxVM, implemented in
Common Lisp. The default is now the Racket-based compiler under
`languages/scheme-racket/`; this one is kept as an alternative
implementation and as a reference.

## Requirements

A Common Lisp implementation. The `run.sh` wrapper supports ABCL, CCL,
CLISP, CMUCL, and SBCL, and defaults to CLISP. Set the `CL_IMPL`
environment variable to pick a different implementation:

```bash
export CL_IMPL=sbcl
```

VeloxVM itself should be built separately (`make` in the repository
root). This compiler produces `.vm` files; the VM in `bin/vm` is what
runs them.

## Usage

Compile a Scheme source file:

```bash
cd languages/scheme
./run.sh input.scm
```

The compiler writes `input.vm` alongside the source file.

## Keeping the primitives table in sync

`compiler.lisp` contains a `vm-symbol-list` constant. Its ordering must
match the VM's core symbol map in `core/vm-symbols.c`, which in turn
must align with the operator table in `core/vm-procedures.c`. The
compiled bytecode refers to primitives by index, so any drift between
these lists will cause compiled programs to invoke the wrong operators
at runtime.

If you add or rename a VM primitive, update:

- `core/vm-procedures.c` — the operator table (source of truth)
- `core/vm-symbols.c` — the core symbol map
- `compiler.lisp` — this compiler's `vm-symbol-list`
- `languages/scheme-racket/primitives.rkt` — the Racket compiler's list
- `languages/python/pyvelox/primitives.py` — the Python compiler's list

## Files

- `compiler.lisp` — the compiler
- `compiler-cli.lisp` — command-line entry point
- `scheme-lib.lisp` — Scheme runtime helpers used during compilation
- `bit-stream.lisp` — bit-level bytecode I/O
- `entropy.lisp`, `fold.lisp`, `log.lisp`, `series.lisp` — support modules
- `sbcl-compile.lisp` — helper for building a standalone SBCL image
- `run.sh` — wrapper that selects the Common Lisp implementation

## See also

- `../scheme-racket/` — default Scheme compiler
- `../../doc/instruction-set.md` — VM instruction reference
- `../../doc/bytecode-format.md` — bytecode specification
