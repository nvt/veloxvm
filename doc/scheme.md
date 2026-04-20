# VeloxVM Scheme

A compiler that translates Scheme programs to VeloxVM bytecode, suitable for
running on resource-constrained IoT devices. The compiler supports most of
R5RS, including `define-syntax`/`syntax-rules` macros and full
quasiquotation, and offers three levels of compile-time optimization. See
[scheme-r5rs-compliance.md](scheme-r5rs-compliance.md) for a feature-by-feature
reference and a concrete list of what is not supported.

## Contents

1. [Installation](#installation)
2. [Quick start](#quick-start)
3. [Command-line usage](#command-line-usage)
4. [Quasiquotation](#quasiquotation)
5. [Macros](#macros)
6. [Derived forms](#derived-forms)
7. [Optimization](#optimization)
8. [Debugging](#debugging)
9. [Best practices](#best-practices)
10. [Troubleshooting](#troubleshooting)
11. [Known limitations](#known-limitations)

## Installation

### Prerequisites

Install Racket 8.0 or later (required to build the compiler):

```bash
# macOS (Homebrew)
brew install racket

# Ubuntu/Debian
sudo apt-get install racket

# Or download from https://racket-lang.org/
```

Then build VeloxVM:

```bash
cd /path/to/veloxvm
make
```

### Verifying the installation

```bash
./compile.sh hello
./bin/vm apps/hello.vm
```

The top-level `compile.sh` wraps the Racket compiler; you can also invoke it
directly with `./compile-racket.sh apps/hello.scm`.

## Quick start

Write `hello.scm`:

```scheme
(define (greet name)
  (print "Hello, ")
  (print name)
  (print "!"))

(greet "World")
```

Compile and run:

```bash
./compile.sh hello
./bin/vm apps/hello.vm
# => Hello, World!
```

## Command-line usage

```bash
./compile-racket.sh [options] <file.scm>
```

Common options:

- `-o <file>` — set the output file (default: `<input>.vm`)
- `-v`, `--verbose` — show compilation details
- `--debug` — enable debug output
- `--opt-level <0-2>` — set the optimization level (default: 1)
- `--no-optimize` — disable optimizations

You can also invoke the compiler directly:

```bash
racket languages/scheme-racket/main.rkt [options] app.scm
```

## Quasiquotation

Quasiquote (`` ` ``), unquote (`,`), and unquote-splicing (`,@`) are fully
supported, including nested quasiquotation:

```scheme
(define x 42)
`(the answer is ,x)          ; => (the answer is 42)

(define nums '(1 2 3))
`(start ,@nums end)          ; => (start 1 2 3 end)

(define (make-adder n)
  `(lambda (x) (+ x ,n)))
(make-adder 5)               ; => (lambda (x) (+ x 5))
```

## Macros

Define macros with `define-syntax` and `syntax-rules`:

```scheme
(define-syntax unless
  (syntax-rules ()
    [(unless test body ...)
     (if (not test) (begin body ...))]))

(unless (< x 0)
  (print "x is non-negative"))
```

### Ellipsis patterns

```scheme
(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and test) test]
    [(my-and test rest ...)
     (if test (my-and rest ...) #f)]))

(my-and #t (> 5 3) (< 2 10))  ; => #t
```

### Literal matching

```scheme
(define-syntax cond
  (syntax-rules (else)
    [(cond (else result ...))
     (begin result ...)]
    [(cond (test result ...))
     (if test (begin result ...))]
    [(cond (test result ...) clause ...)
     (if test
         (begin result ...)
         (cond clause ...))]))
```

### Local macros

`let-syntax` and `letrec-syntax` provide lexically-scoped macros:

```scheme
(let-syntax ([when (syntax-rules ()
                     [(when test body ...)
                      (if test (begin body ...))])])
  (when (> x 10)
    (print "x is large")))
```

### Hygiene caveat

Macro hygiene is not fully enforced, so pattern variables can clash with
identifiers in the macro's template. Use distinctive names for macro-internal
bindings to avoid capture:

```scheme
(define-syntax good
  (syntax-rules ()
    [(good expr)
     (let ((%tmp expr))
       %tmp)]))
```

## Derived forms

All R5RS derived forms except `delay` are supported:

```scheme
(let ((x 1) (y 2))
  (+ x y))

(let* ((x 1)
       (y (+ x 1)))
  (* x y))

(letrec ((even? (lambda (n)
                  (or (= n 0) (odd? (- n 1)))))
         (odd?  (lambda (n)
                  (and (not (= n 0)) (even? (- n 1))))))
  (even? 88))

(cond
  [(< x 0) "negative"]
  [(= x 0) "zero"]
  [else    "positive"])

(case x
  [(1 2 3) "small"]
  [(4 5 6) "medium"]
  [else    "large"])

(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((> i 10) sum))
```

## Optimization

Three optimization levels are available; level 1 is the default.

- **Level 0** (`--opt-level 0`) — no optimizations. Use when comparing
  compiler output against the source.
- **Level 1** (default) — constant folding and identity simplification.
- **Level 2** (`--opt-level 2`) — adds more aggressive algebraic
  simplifications and dead-branch elimination.

Examples of what the optimizer does:

```scheme
(+ 1 2)           ; => 3   (constant folding)
(+ x 0)           ; => x   (identity)
(* y 1)           ; => y   (identity)
(* z 0)           ; => 0
(if #t a b)       ; => a   (dead branch)
(not #t)          ; => #f
```

## Debugging

Enable debug output with `--debug`:

```bash
./compile-racket.sh --debug app.scm
```

Typical output:

```
DEBUG: Compiled 2 top-level expressions
DEBUG: Accumulated 9 bytes total
DEBUG: bc has 1 expressions
DEBUG: Expression 0: 9 bytes
```

### Debugging strategies

- Start with a minimal reproduction.
- Add `(print ...)` to trace execution at runtime.
- Compile with `--no-optimize` to rule out an optimizer bug.
- Use `--debug` to see how macros expanded.

## Best practices

- Define macros before using them; keep them near the top of a file.
- Prefer macros over runtime abstractions when the form is fixed at compile
  time — they have no runtime cost.
- Enable optimizations (the default level 1) for anything you run on a
  device.
- Test with and without optimizations to catch optimizer-sensitive bugs.

## Troubleshooting

**Compilation fails**: check the syntax with `--debug`; simplify the source
until it compiles, then bisect.

**Program behaves differently than expected**: disable optimizations with
`--no-optimize`. If the behavior changes, the optimizer may be at fault.

**Macro doesn't expand**: make sure `define-syntax` is at the top level, and
check the pattern against simpler test inputs first.

## Known limitations

The following R5RS features are not supported. See
[scheme-r5rs-compliance.md](scheme-r5rs-compliance.md) for the full list.

- `delay` and `force` — the VM does not implement promises.
- Symbol manipulation at runtime (`symbol?`, `symbol->string`,
  `string->symbol`) — symbols are internal to the VM and not exposed to user
  code.
- `call-with-input-file` and `call-with-output-file` — use
  `with-input-from-file` / `with-output-to-file`, or open and close the port
  manually.
- `scheme-report-environment`, `null-environment`,
  `interaction-environment` — VeloxVM uses a single global environment.
- Complex numbers — intentionally omitted for code-size reasons.
- `rationalize` — not implemented.
- Full macro hygiene — identifiers introduced by a macro can collide with
  user identifiers.

## References

- [scheme-r5rs-compliance.md](scheme-r5rs-compliance.md) — feature-by-feature
  reference and runtime library.
- [scheme-internals.md](scheme-internals.md) — compiler architecture for
  contributors.
- R5RS specification: <http://www.schemers.org/Documents/Standards/R5RS/>
