;;; VeloxVM Unit Tests - R7RS string->symbol (restricted form)
;;;
;;; The implementation looks up the string in the program's symbol
;;; table (populated at load time) and returns the corresponding
;;; symbol. Names not already in the table raise a VM-level error
;;; that is not catchable by guard; the unrestricted R7RS semantics
;;; require runtime symbol-table extension and are not implemented.

(include "../unit-test-framework.scm")

(test-suite "R7RS string->symbol (restricted)")

;; Symbols that appear elsewhere in this file land in the program's
;; symbol table at compile time, so string->symbol can resolve them.
;; Reference each name once so the compiler emits it.

(define foo 'foo)
(define bar 'bar)
(define baz 'baz)
(define hello 'hello)

;; Round-trip through symbol->string and back
(assert-equal 'foo (string->symbol (symbol->string 'foo))
              "round-trip foo")
(assert-equal 'bar (string->symbol (symbol->string 'bar))
              "round-trip bar")

;; symbol? predicate confirms the result type
(assert-equal #t (symbol? (string->symbol "foo"))
              "result is a symbol")
(assert-equal #f (string? (string->symbol "foo"))
              "result is not a string")

;; eq? identity: looking up the same name twice returns the same
;; symbol (interned)
(assert-equal #t (eq? (string->symbol "hello") (string->symbol "hello"))
              "interning: same name, same symbol")
(assert-equal #t (eq? (string->symbol "hello") 'hello)
              "interning: matches the literal symbol")

;; Different names produce different symbols
(assert-equal #f (eq? (string->symbol "foo") (string->symbol "bar"))
              "different names, different symbols")

;; The symbols mentioned in this file are findable
(assert-equal 'baz (string->symbol "baz") "baz looked up")
(assert-equal 'hello (string->symbol "hello") "hello looked up")

;; The unknown-name failure path raises VM_ERROR_SYMBOL_UNDEFINED,
;; which terminates the thread rather than producing a catchable
;; condition. The success-only assertions above cover the supported
;; behaviour; documenting the failure path here in lieu of a runtime
;; assertion.

(test-summary)
