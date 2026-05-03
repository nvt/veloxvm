;;; VeloxVM Unit Tests - R7RS list helpers
;;; Tests for: list-copy, list-tabulate.

(include "../unit-test-framework.scm")

;; Inline definitions from languages/scheme-racket/runtime/r7rs-lists.scm
(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

(define (list-tabulate n proc)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons (proc i) acc)))))

(test-suite "R7RS list helpers")

;; list-copy
(assert-equal '() (list-copy '()) "list-copy of empty list")
(assert-equal '(1 2 3) (list-copy '(1 2 3)) "list-copy preserves contents")
(assert-equal '(a b c) (list-copy '(a b c)) "list-copy with symbols")

(define original '(a b c))
(define copied (list-copy original))
(assert-equal '(a b c) copied "list-copy result matches original")
(assert-equal #t (not (eq? original copied))
              "list-copy returns a fresh top-level cell")

;; list-tabulate
(assert-equal '() (list-tabulate 0 (lambda (i) i))
              "list-tabulate with n=0 is empty")
(assert-equal '(0) (list-tabulate 1 (lambda (i) i))
              "list-tabulate with n=1")
(assert-equal '(0 1 2 3 4) (list-tabulate 5 (lambda (i) i))
              "list-tabulate of identity")
(assert-equal '(0 1 4 9 16) (list-tabulate 5 (lambda (i) (* i i)))
              "list-tabulate of squares")
(assert-equal '(10 11 12) (list-tabulate 3 (lambda (i) (+ 10 i)))
              "list-tabulate with offset")

(test-summary)
