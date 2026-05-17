# VeloxVM Common Lisp Scheme Compiler (archived)

The original Scheme-to-bytecode compiler for VeloxVM, implemented in
Common Lisp. **This compiler is archived: it is preserved as a
historical reference and is no longer maintained.** The active Scheme
front-end is the Racket-based compiler under `languages/scheme-racket/`.

## Current state and last working VM

The compiler emits bytecode format version 1 (see
`vm-bytecode-version` in `compiler.lisp`). The contents of this
directory are held at their state from master commit `6f3c0a3`
(2026-04-20, "Merge pull request \#34 from nvt/contiki-ng-default-heap-sizes"),
which is the last master commit where the VM was still on
`VM_BYTECODE_VERSION 1` and the CL compiler's primitive table
(`vm-symbol-list`) matched the VM's operator table. The archive
therefore pairs with a VeloxVM built at `6f3c0a3` or earlier; the
current VM (`VM_BYTECODE_VERSION 4`) rejects its output with
`unsupported bytecode version 1`.

## Why no longer maintained

The active Scheme front-end is the Racket compiler under
`languages/scheme-racket/`. Forward-porting this compiler to the
current bytecode format would require implementing the structural
changes added in versions 2, 3, and 4 (16-bit string/symbol
tables, vector literals, the `include` directive, and the
per-program captures section), then re-adding entries to
`vm-symbol-list` for every primitive introduced since `6f3c0a3`.
That work has not been done and is not planned.

## Using the archive

To exercise this compiler end-to-end, check out master at
`6f3c0a3` (or any earlier v1 commit), build the VM from there
(`make` at the repository root), and invoke `./run.sh` here. This
combination was verified on 2026-05-16 against the example apps
under `apps/` (factorial, binomial, let-lambda, sieve); it has not
been continuously tested and the CL compiler at that commit may
still have its own unrelated bugs.

The primitive-sync invariant in `CLAUDE.md` and
`doc/r7rs-implementation-plan.md` excludes this compiler.

## Layout

- `compiler.lisp` — the compiler
- `compiler-cli.lisp` — command-line entry point
- `scheme-lib.lisp` — Scheme runtime helpers used during compilation
- `bit-stream.lisp` — bit-level bytecode I/O
- `entropy.lisp`, `fold.lisp`, `log.lisp`, `series.lisp` — support modules
- `sbcl-compile.lisp` — helper for building a standalone SBCL image
- `run.sh` — wrapper that selects the Common Lisp implementation
  (ABCL, CCL, CLISP, CMUCL, SBCL; defaults to CLISP)

## See also

- `../scheme-racket/` — the active Scheme compiler
- `../../doc/bytecode-format.md` — current bytecode specification
- `../../doc/instruction-set.md` — VM instruction reference
