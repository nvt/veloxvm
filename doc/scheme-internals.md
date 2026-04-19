# VeloxVM Scheme Compiler — Internals

This document describes the internal architecture of the VeloxVM Scheme
compiler for contributors who want to understand, maintain, or extend it.

## Pipeline overview

The compiler is written in Racket and organized as a pipeline of independent
modules:

```
Source -> Reader -> Expander -> Rewriter -> Optimizer -> Compiler -> Bytecode -> .vm file
```

Each stage transforms its input into a form suitable for the next stage.

## Directory layout

```
languages/scheme-racket/
|-- main.rkt          Entry point, CLI argument parsing
|-- reader.rkt        S-expression parser
|-- expander.rkt      Macro expansion engine
|-- rewriter.rkt      Derived-form rewriting
|-- optimizer.rkt     Optimization passes
|-- compiler.rkt      Bytecode generation
|-- bytecode.rkt      Bytecode encoding and file writing
|-- errors.rkt        Error reporting
|-- primitives.rkt    VM primitive definitions
|-- utils.rkt         Utility functions
|-- runtime/
|   `-- r5rs-io.scm   Runtime library (call-with-*-file)
`-- tests/
    |-- run-all-tests.rkt
    |-- test-macros.rkt, test-reader.rkt, test-rewriter.rkt,
    |-- test-optimizer.rkt, test-compiler.rkt, test-bytecode.rkt,
    `-- test-end-to-end.rkt
```

## The pipeline stages

### Reader (`reader.rkt`)

Turns source text into S-expressions. Handles literals (numbers, strings,
characters, booleans), quote abbreviations (`'x` -> `(quote x)`), and
quasiquote abbreviations (`` `x `` -> `(quasiquote x)`).

Key entry points: `read-from-file`, `read-from-string`.

### Expander (`expander.rkt`)

Expands macros and quasiquotation. At exit there are no more `define-syntax`
bindings, no ` ``...`` ` forms, and all `include` directives have been
inlined.

Responsibilities:

- Track macro definitions (`define-syntax`, `let-syntax`, `letrec-syntax`)
- Pattern matching for `syntax-rules`
- Ellipsis expansion and literal matching
- Nested quasiquote handling
- `include` directive processing

Key entry points: `expand-expr`, `expand-macro`, `expand-quasiquote`,
`process-include`.

Macro environments are a stack: a global hash table holds top-level
`define-syntax` bindings; `let-syntax`/`letrec-syntax` push a lexical frame
for the body's extent.

### Rewriter (`rewriter.rkt`)

Rewrites R5RS derived forms into the compiler's primitive forms. Examples:

```scheme
(cond [(> x 0) 'pos] [(< x 0) 'neg] [else 'zero])
;; -> (if (> x 0) 'pos (if (< x 0) 'neg 'zero))

(let ([x 1] [y 2]) (+ x y))
;; -> ((lambda (x y) (+ x y)) 1 2)

(letrec ([f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))])
  (f 5))
;; -> (let ([f #f]) (set! f (lambda (n) ...)) (f 5))
```

Key entry points: `rewrite-expr`, `rewrite-cond`, `rewrite-let`.

### Optimizer (`optimizer.rkt`)

Applies three optimization levels:

- **Level 0** — no optimization.
- **Level 1** (default) — constant folding and identity simplification.
- **Level 2** — adds algebraic simplifications and dead-branch elimination.

Representative rewrites:

```scheme
;; Constant folding
(+ 1 2)           -> 3
(string-length "hello") -> 5

;; Identities
(+ x 0)           -> x
(* z 0)           -> 0

;; Control flow
(if #t c a)       -> c
(begin expr)      -> expr
```

Key entry points: `optimize-expr`, `fold-constants`, `simplify-identity`.

### Compiler (`compiler.rkt`)

Translates core Scheme expressions into VM bytecode operations. The VM is
stack-based; each Scheme form maps to a VM form or a sequence of them:

| Scheme form | VM form           |
|-------------|-------------------|
| `define`    | `VM_FORM_DEFINE`  |
| `lambda`    | `VM_FORM_LAMBDA`  |
| `if`        | `VM_FORM_IF`      |
| `set!`      | `VM_FORM_SET`     |
| `begin`     | `VM_FORM_BEGIN`   |
| primitives  | `VM_FORM_PRIMITIVE` with an operation ID |

Variable references are emitted as bare symbol references, which the VM
resolves at runtime by walking the thread expression stack for a
matching binding (see `vm_symbol_resolve` in `core/vm-symbols.c`). This
means closures do not carry a captured environment — a limitation
documented in [scheme-r5rs-compliance.md](scheme-r5rs-compliance.md#closure-capture).

Key entry points: `compile-expr`, `compile-define`, `compile-lambda`,
`compile-application`.

### Bytecode encoder (`bytecode.rkt`)

Encodes compiled expressions into VeloxVM's binary format. The file
starts with a 3-byte header (two magic bytes followed by a bytecode
version), followed by three variable-length tables: string table,
symbol table, and expression table. See
[bytecode-format.md](bytecode-format.md) for the full format
specification.

Key entry points: `encode-bytecode`, `encode-type`, `write-bytecode-file`.

### Error reporting (`errors.rkt`)

Formats messages with a short description, the offending expression, and the
source file. Error categories: syntax, expansion, compilation, optimization.

## Testing

Tests live in `languages/scheme-racket/tests/`, one file per stage plus an
end-to-end suite:

```bash
cd languages/scheme-racket/tests
racket run-all-tests.rkt
```

Run a single suite directly:

```bash
racket test-macros.rkt
racket test-optimizer.rkt
```

All tests should pass before merging changes. Coverage roughly follows the
pipeline stages: macro expansion, reader, rewriter, optimizer, compiler,
bytecode encoding, and full programs end-to-end.

New tests follow the RackUnit form:

```racket
(test-case "description"
  (check-equal? (actual-result) (expected-result)))
```

## Programmatic usage

```racket
#lang racket

(require "languages/scheme-racket/main.rkt")

(compile-file "input.scm" "output.vm")

;; From a string
(define bytecode (compile-string "(define x 42) (print x)"))
(write-bytecode-file "output.vm" bytecode)

;; With parameters
(parameterize ([current-optimization-level 2]
               [current-debug-mode #t])
  (compile-file "input.scm" "output.vm"))
```

Parameters:

```racket
(current-optimization-level 2)   ; 0, 1, or 2
(current-debug-mode #t)
(current-include-paths '("." "lib/" "/usr/local/lib/veloxvm/"))
```

Custom pipelines can be composed from the individual modules:

```racket
(require "languages/scheme-racket/reader.rkt")
(require "languages/scheme-racket/expander.rkt")
(require "languages/scheme-racket/optimizer.rkt")
(require "languages/scheme-racket/compiler.rkt")

(define (my-compile source)
  (let* ([exprs     (read-from-string source)]
         [expanded  (map expand-expr exprs)]
         [optimized (map optimize-expr expanded)])
    (compile-expressions optimized)))
```

## Contributing

### Adding an optimization

1. Add the transform to `optimizer.rkt`.
2. Add test cases to `test-optimizer.rkt`.
3. Decide which optimization level it belongs to and wire it up.
4. Document it in this file.

### Adding a primitive

1. Register it in `primitives.rkt`.
2. Add a compilation case in `compiler.rkt` (if it needs more than the
   default primitive-application path).
3. Add tests.

### Adding a derived form

1. Add a rewrite rule in `rewriter.rkt`.
2. Add tests in `test-rewriter.rkt`.
3. Document the form in [scheme.md](scheme.md).

### Code style

Follow the Racket conventions already used in the codebase: descriptive
names, short functions, and a docstring for anything another module is
expected to call.

## Debugging the compiler

### Debug mode

```bash
./compile-racket.sh --debug program.scm
```

Shows expression count, total bytecode size, per-expression sizes, and any
optimization decisions the compiler logs.

### Inspecting pipeline stages

```racket
(define source "(define x (+ 1 2))")

(displayln (read-from-string source))
(displayln (expand-expr    (first (read-from-string source))))
(displayln (rewrite-expr   (expand-expr ...)))
(displayln (optimize-expr  (rewrite-expr ...)))
```

### Typical issues

- **Macro expansion loops** — look for a recursive `syntax-rules` that never
  terminates its recursion.
- **Optimizer bug** — compile with `--no-optimize` and compare output.
- **Bytecode too large** — raise the optimization level or look for missed
  constant-folding opportunities.

## References

- R5RS specification: <http://www.schemers.org/Documents/Standards/R5RS/>
- VeloxVM bytecode format: [bytecode-format.md](bytecode-format.md)
- VeloxVM instruction set: [instruction-set.md](instruction-set.md)
- Racket documentation: <https://docs.racket-lang.org/>
