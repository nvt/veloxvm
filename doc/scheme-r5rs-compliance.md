# VeloxVM Scheme — R5RS Compliance Reference

VeloxVM Scheme implements most of R5RS (the *Revised^5 Report on the
Algorithmic Language Scheme*, 20 February 1998). This document enumerates
the concrete features that are and are not supported. Gaps we know about
are listed up front under [Not supported](#not-supported); the rest of
the document breaks down each R5RS section. Section references (e.g.
§4.2, §6.6) point at the corresponding chapters of R5RS.

## Contents

1. [Status at a glance](#status-at-a-glance)
2. [Not supported](#not-supported)
3. [Partially supported](#partially-supported)
4. [Primitive expressions](#primitive-expressions)
5. [Derived expressions](#derived-expressions)
6. [Macros](#macros)
7. [Equivalence predicates](#equivalence-predicates)
8. [Numbers](#numbers)
9. [Booleans](#booleans)
10. [Pairs and lists](#pairs-and-lists)
11. [Symbols](#symbols)
12. [Characters](#characters)
13. [Strings](#strings)
14. [Vectors](#vectors)
15. [Control features](#control-features)
16. [Input/Output](#inputoutput)
17. [System interface](#system-interface)
18. [Evaluation](#evaluation)
19. [Runtime library](#runtime-library)

## Status at a glance

| R5RS area | R5RS § | Status | Notes |
|-----------|--------|--------|-------|
| Primitive forms | 4.1 | Supported | `lambda`, `if`, `set!`, `quote`, procedure calls, variable references |
| Definitions | 5 | Supported | `define` at top level and in internal definitions |
| Derived forms | 4.2 | Supported except `delay` | `cond`, `case`, `and`, `or`, `let`, `let*`, `letrec`, `begin`, `do`, named `let`, quasiquote |
| Macros | 4.3, 5.3 | Supported | `define-syntax`, `syntax-rules`, `let-syntax`, `letrec-syntax` (hygiene not fully enforced) |
| Equivalence | 6.1 | Supported | `eq?`, `eqv?`, `equal?` |
| Numbers | 6.2 | Partial | Integers and reals only; no complex numbers, no `rationalize` |
| Booleans | 6.3.1 | Supported | |
| Pairs & lists | 6.3.2 | Supported | All composite `cXXXr` forms (up to 4 levels) included |
| Symbols | 6.3.3 | Not exposed to programs | `symbol?`, `symbol->string`, `string->symbol` not available at runtime |
| Characters | 6.3.4 | Supported | |
| Strings | 6.3.5 | Supported | |
| Vectors | 6.3.6 | Supported | |
| Control features | 6.4 | Partial | `dynamic-wind` is a stub; `force` absent |
| Continuations | 6.4 | Supported | `call-with-current-continuation` / `call/cc` |
| Eval | 6.5 | Not supported | `eval` returns an unimplemented error; no environment objects |
| I/O | 6.6 | Partial | See I/O section below |
| System interface | 6.6.4 | Partial | `load` supported; `transcript-on`/`transcript-off` optional and absent |
| Lazy evaluation | 4.2.5, 6.4 | Not supported | `delay`, `force` |

## Not supported

The following are not available in VeloxVM Scheme. In most cases the reason
is a VM architecture decision (to save code size on constrained devices).

- **Lazy evaluation**: `delay`, `force`. The VM does not implement promises.
- **Runtime symbol manipulation**: `symbol?`, `symbol->string`,
  `string->symbol`. Symbols exist inside the VM for procedure lookup but are
  not exposed to programs. Use strings for dynamic identifiers.
- **Environment reification**: `scheme-report-environment`,
  `null-environment`, `interaction-environment`. VeloxVM has a single global
  environment.
- **Complex numbers**: `make-rectangular`, `make-polar`, `real-part`,
  `imag-part`, `magnitude`, `angle`. Intentionally omitted for code-size
  reasons.
- **`rationalize`**: not implemented.
- **`call-with-input-file`, `call-with-output-file`**: not built in; a
  runtime-library version without exception cleanup is shipped (see
  [Runtime library](#runtime-library)). Alternatively, use
  `with-input-from-file` / `with-output-to-file`.
- **`transcript-on`, `transcript-off`**: optional in R5RS, not implemented.
- **Full macro hygiene**: the expander does not rename introduced
  identifiers, so macro-internal bindings can collide with user bindings.
- **`eval` and environment objects**: `eval` is present as a primitive
  but returns an unimplemented error at runtime;
  `scheme-report-environment`, `null-environment`, and
  `interaction-environment` are absent.
- **Closure capture for inner lambdas**: a lambda that references a
  variable bound in an enclosing lambda emits code that cannot resolve
  the reference at runtime. This blocks the classic closure idiom and
  also affects any rewrite that wraps user code in a lambda (`let`,
  `let*`, `case`, named `let`) whenever the body reads an outer variable.
  See [Closure capture](#closure-capture) for a minimal reproduction.

## Partially supported

- **`dynamic-wind`**: currently a stub that does not invoke the `before` or
  `after` thunks. Existing callers that rely on those side effects will not
  observe them.
- **Port constructors**: `call-with-input-file` and `call-with-output-file`
  are available only through the runtime library and do not guarantee port
  cleanup on exceptions.
- **System interface**: only `load` is available.

## Primitive expressions

All primitive expression types in R5RS §4.1 are supported.

| R5RS (§4.1) | Implementation |
|-------------|----------------|
| Variable references | `compiler.rkt` |
| `quote` / `'datum` | VM `op_quote` |
| Procedure calls | Inline forms |
| `lambda` | `VM_FORM_LAMBDA` |
| `if` | VM `op_if` |
| `set!` | VM `op_set` |

`define` (R5RS §5.2) is supported both at top level and as an internal
definition inside a `(body)`.

## Derived expressions

All R5RS §4.2 derived forms are supported except `delay`.

| R5RS form | R5RS § | Method |
|-----------|--------|--------|
| `cond` | 4.2.1 | Rewritten to nested `if` |
| `case` | 4.2.1 | Rewritten to `let` + `memv` + `cond` |
| `and` | 4.2.1 | VM `op_and`, short-circuit |
| `or` | 4.2.1 | VM `op_or`, short-circuit |
| `let` | 4.2.2 | Rewritten to `lambda` application |
| `let*` | 4.2.2 | Rewritten to nested `let` |
| `letrec` | 4.2.2 | Rewritten to `let` with `set!` |
| `begin` | 4.2.3 | VM `op_begin` |
| `do` | 4.2.4 | Rewritten to a recursive helper |
| Named `let` | 4.2.4 | Via `letrec` |
| `delay` | 4.2.5 | **Not supported** |
| Quasiquote (`` ` ``, `,`, `,@`) | 4.2.6 | Handled by the expander (full nesting) |

## Macros

R5RS §4.3 and §5.3 describe the macro facility. `define-syntax` and
`syntax-rules` are implemented, including ellipsis patterns, literal
matching, and recursive macros. `let-syntax` and `letrec-syntax` are
supported for locally scoped macros. Hygiene is not fully enforced — see
[scheme.md](scheme.md#hygiene-caveat).

| R5RS form | R5RS § | Status |
|-----------|--------|--------|
| `define-syntax` | 5.3 | Supported |
| `syntax-rules` | 4.3.2 | Supported |
| `let-syntax` | 4.3.1 | Supported |
| `letrec-syntax` | 4.3.1 | Supported |

Example:

```scheme
(define-syntax when
  (syntax-rules ()
    [(when test body ...)
     (if test (begin body ...))]))

(define-syntax my-let
  (syntax-rules ()
    [(my-let ([var val] ...) body ...)
     ((lambda (var ...) body ...) val ...)]))
```

## Equivalence predicates

Per R5RS §6.1.

| R5RS | VM primitive |
|------|--------------|
| `eqv?` | `op_eqvp` |
| `eq?` | `op_eqp` |
| `equal?` | `op_equalp` |

## Numbers

Covers R5RS §6.2. The numeric tower is implemented as integers and reals
(flonums); complex numbers are omitted.

### Type predicates

| R5RS | Implementation |
|------|----------------|
| `number?`, `complex?`, `real?`, `rational?`, `integer?` | VM primitives |
| `exact?`, `inexact?` | VM primitives |
| `zero?` | Rewritten to `(= x 0)` |

R5RS §6.2.5 notes that implementations without complex numbers can define
`complex?` as equivalent to `real?` (or `number?`), which is what VeloxVM
does — the predicate exists even though complex values cannot be
constructed.

### Comparisons and predicates

| R5RS | Implementation |
|------|----------------|
| `=`, `<`, `>`, `<=`, `>=` | VM primitives |
| `positive?`, `negative?` | Rewritten |
| `odd?`, `even?` | Rewritten via `remainder` |
| `max`, `min` | Rewritten (binary) |

### Arithmetic

| R5RS | Implementation |
|------|----------------|
| `+`, `-`, `*`, `/` | VM primitives |
| `abs` | Rewritten via `if` |
| `quotient`, `remainder`, `modulo` | VM primitives |
| `gcd`, `lcm` | VM primitives |
| `numerator`, `denominator` | VM primitives |

Special cases from R5RS §6.2.5 are observed: `(- z)` and `(/ z)` (optional
in R5RS) return the additive and multiplicative inverse respectively;
`(gcd)` returns 0; `(lcm)` returns 1; and `modulo` returns a result with
the sign of the divisor.

### Transcendental functions

All require `VM_ENABLE_REALS`:

| R5RS | Status |
|------|--------|
| `floor`, `ceiling`, `truncate`, `round` | Supported |
| `exp`, `log`, `sin`, `cos`, `tan` | Supported |
| `asin`, `acos`, `atan` | Supported |
| `sqrt`, `expt` | Supported |
| `rationalize` | **Not supported** |

### Number/string conversion

| R5RS | VM primitive |
|------|--------------|
| `number->string` | `op_number_to_string` |
| `string->number` | `op_string_to_number` |

### Complex numbers

Not supported. `make-rectangular`, `make-polar`, `real-part`, `imag-part`,
`magnitude`, and `angle` are all absent.

## Booleans

Per R5RS §6.3.1. Only `#f` counts as false; every other value — including
`0`, `#t`, `'()`, and empty strings/vectors — counts as true.

| R5RS | VM primitive |
|------|--------------|
| `not` | `op_not` |
| `boolean?` | `op_booleanp` |

## Pairs and lists

All R5RS §6.3.2 list operations are supported:

| Category | Procedures |
|----------|------------|
| Constructors | `cons`, `list` |
| Accessors | `car`, `cdr`, and all 28 composite `cXXXr` forms (2–4 levels) |
| Mutators | `set-car!`, `set-cdr!` |
| Predicates | `pair?`, `null?`, `list?` |
| Operations | `length`, `append`, `reverse` |
| Access | `list-ref`, `list-tail` |
| Search | `memq`, `memv`, `member` |
| Association | `assq`, `assv`, `assoc` |

The composite `cXXXr` family (`caar` through `cddddr`) is provided by the
rewriter.

## Symbols

R5RS §6.3.3 describes symbols as first-class objects that can be compared
with `eqv?` and converted to/from strings. In VeloxVM Scheme, runtime
symbol manipulation is **not supported**: `symbol?`, `symbol->string`, and
`string->symbol` are not exposed to programs. Symbols exist inside the VM
for procedure dispatch but are not first-class at the Scheme level. Use
strings if you need to build identifiers dynamically.

## Characters

All R5RS §6.3.4 character operations are supported, in most cases through a
rewrite that delegates to `char-compare` or a simple range check:

| Category | Procedures |
|----------|------------|
| Predicate | `char?` |
| Comparison | `char=?`, `char<?`, `char>?`, `char<=?`, `char>=?` |
| Case-insensitive | `char-ci=?` and the other four comparisons |
| Classification | `char-alphabetic?`, `char-numeric?`, `char-whitespace?` |
| Case predicates | `char-upper-case?`, `char-lower-case?` |
| Conversion | `char->integer`, `integer->char` |
| Case conversion | `char-upcase`, `char-downcase` |

## Strings

All R5RS §6.3.5 string operations are supported:

| Category | Procedures |
|----------|------------|
| Predicate | `string?` |
| Constructor | `make-string`, `string` |
| Access | `string-length`, `string-ref` |
| Mutator | `string-set!`, `string-fill!` |
| Comparison | `string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` |
| Case-insensitive | `string-ci=?` and the other four comparisons |
| Operations | `substring`, `string-append`, `string-copy` |
| Conversion | `string->list`, `list->string` |

## Vectors

Per R5RS §6.3.6.

| R5RS | VM primitive |
|------|--------------|
| `vector?` | `op_vectorp` |
| `make-vector`, `vector` | VM primitives |
| `vector-length` | `op_vector_length` |
| `vector-ref`, `vector-set!` | VM primitives |
| `vector->list`, `list->vector` | VM primitives |
| `vector-fill!` | `op_vector_fill` |

## Control features

Per R5RS §6.4.

| R5RS | Status |
|------|--------|
| `procedure?` | Supported |
| `apply` | Supported |
| `map`, `for-each` | Supported |
| `call-with-current-continuation` / `call/cc` | Supported |
| `values`, `call-with-values` | Supported |
| `dynamic-wind` | **Stub** — does not invoke the before/after thunks |
| `delay`, `force` | **Not supported** |

### Proper tail recursion (§3.5)

R5RS §3.5 requires Scheme implementations to be properly tail-recursive
in every tail position. The test suite
`tests/unit-tests/r5rs/test-tail-calls.scm` exercises each tail position
with 50,000 recursive calls against a `VM_CONTEXT_STACK_SIZE` of 64 —
large enough that a non-TCO recursion blows the stack decisively. The
following positions pass:

- `if` consequent and alternate
- `cond` clause
- `and` last expression
- `or` last expression
- `when` body (library-provided derived form)
- `let`, `let*`, and named `let` bodies (when the body does not reference
  an outer lexical variable — see [Closure capture](#closure-capture)
  below)
- `begin` last expression
- Mutual (indirect) recursion
- `apply` in tail position

The remaining R5RS tail positions — `case` clauses and `letrec` bodies
— cannot currently be verified, because a separate closure-capture bug
makes those forms hang on any recursive program before TCO is reached.
See the next section.

### Closure capture

An unrelated defect affects inner lambdas: when a lambda references a
variable from an enclosing lambda's scope, that reference is compiled as
a symbol lookup that does not find the enclosing binding. Concretely,

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))
((make-adder 5) 3)   ; error: Undefined symbol: n
```

reports `Undefined symbol: n` at runtime. The same defect causes

```scheme
(define (counter n)
  (let ((tmp n))
    (if (= n 0) 'ok (counter (- n 1)))))
```

to loop forever: the rewriter expands `let` into `((lambda (tmp) ...) n)`,
and the inner lambda's references to `n` are not captured, so the
recursive call never sees `n` change. Programs that happen not to refer
to enclosing-scope variables from inside a lambda body are unaffected,
which is why the majority of the test suite still passes. This is a
significant gap and is tracked separately from the TCO claim above.

## Input/Output

Per R5RS §6.6.

### Ports (§6.6.1)

| R5RS | Status |
|------|--------|
| `input-port?`, `output-port?` | Supported |
| `current-input-port`, `current-output-port` | Supported |
| `with-input-from-file`, `with-output-to-file` | VM primitives (both optional in R5RS) |
| `open-input-file`, `open-output-file` | Supported |
| `close-input-port`, `close-output-port` | Supported (shared `op_close_port`) |
| `call-with-input-file`, `call-with-output-file` | **Runtime library only** (no exception cleanup) |

### Input operations (§6.6.2)

| R5RS | VM primitive |
|------|--------------|
| `read` | `op_read` |
| `read-char` | `op_read_char` |
| `peek-char` | `op_peek_char` |
| `eof-object?` | `op_eof_objectp` |
| `char-ready?` | `op_char_readyp` |

### Output operations (§6.6.3)

| R5RS | Implementation |
|------|----------------|
| `write` | VM `op_write` |
| `display` | Rewritten to `print` |
| `newline` | Rewritten to `(write-char #\newline)` |
| `write-char` | VM `op_write_char` |

## System interface

Per R5RS §6.6.4. All procedures in this section are marked *optional* in
R5RS.

| R5RS | Status |
|------|--------|
| `load` | Supported (but loads compiled bytecode, not source) |
| `transcript-on`, `transcript-off` | Not supported |

VeloxVM also provides IoT-specific system functions that are outside the
scope of R5RS.

## Evaluation

Per R5RS §6.5.

| R5RS | Status |
|------|--------|
| `eval` | Not supported (signals an unimplemented error) |
| `scheme-report-environment` | Not supported |
| `null-environment` | Not supported |
| `interaction-environment` (optional) | Not supported |

VeloxVM uses a single global environment, so environment reification is not
applicable.

## Runtime library

The runtime library provides R5RS procedures implemented in Scheme rather
than in the VM core. It lives at `languages/scheme-racket/runtime/` and
holds one file:

- `r5rs-io.scm` — `call-with-input-file` and `call-with-output-file`.

### `r5rs-io.scm` — file I/O

```scheme
(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (let ((value (proc port)))
      (close-input-port port)
      value)))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (let ((value (proc port)))
      (close-output-port port)
      value)))
```

#### Usage

Because the `include` directive currently has issues in nested scopes, the
recommended approach is to copy the two definitions into your program:

```scheme
(define (call-with-output-file filename proc) ...)
(define (call-with-input-file filename proc) ...)

(call-with-output-file "data.txt"
  (lambda (port)
    (display "Hello, World!" port)))
```

Alternatively, `(include "languages/scheme-racket/runtime/r5rs-io.scm")`
at the top level works.

#### Limitations

- **No exception cleanup**: ports are closed only after normal completion.
  Guaranteed cleanup on exceptions is not provided, because the
  VM's `dynamic-wind` is currently a stub.
- **Portable across ports**: relies only on `open-input-file`,
  `open-output-file`, `close-input-port`, and `close-output-port`, which
  are available on both POSIX and Contiki-NG.

## References

- R5RS specification: <http://www.schemers.org/Documents/Standards/R5RS/>
- Compiler architecture: [scheme-internals.md](scheme-internals.md)
- User guide: [scheme.md](scheme.md)
