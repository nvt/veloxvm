;;; VeloxVM Unit Tests - R7RS library system
;;; Exercises define-library / import / export lowering. This is a
;;; flat, permissive lowering (see doc/r7rs-library-system-design.md):
;;; library bodies are spliced into the single namespace, standard
;;; imports are no-ops, and imports validate the name but do not hide
;;; un-imported bindings.

(include "../unit-test-framework.scm")

;; A standard-import library: (scheme base) resolves to existing
;; primitives/prelude, so importing it is a no-op that must still compile.
(define-library (test math)
  (import (scheme base))
  (export square cube)
  (begin
    (define (square x) (* x x))
    (define (cube x) (* x x x))))

;; Cross-library dependency: chain-b imports chain-a. Emission order is
;; topologically sorted so chain-a precedes chain-b.
(define-library (test chain-a)
  (export inc)
  (begin (define (inc x) (+ x 1))))

(define-library (test chain-b)
  (import (test chain-a))
  (export add2)
  (begin (define (add2 x) (inc (inc x)))))

;; Two exports; we import with (only ...) to check permissive resolution.
(define-library (test pair-exports)
  (export a b)
  (begin (define a 1) (define b 2)))

(import (test math))
(import (test chain-b))
(import (only (test pair-exports) a))

(test-suite "R7RS library system")

(assert-equal 36 (square 6) "imported procedure (square)")
(assert-equal 27 (cube 3)   "imported procedure (cube)")
(assert-equal 12 (add2 10)  "cross-library import (chain-b uses chain-a)")
(assert-equal 1  a          "(only ...) brings in the selected name")
(assert-equal 2  b          "permissive: un-selected export still visible")

(test-summary)
