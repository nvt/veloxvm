;;; ============================================================================
;;; VeloxVM R7RS List Helpers Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small list helpers that don't shadow VM primitives. The
;;; comparator-aware variants of assoc and member specified by R7RS
;;; (third argument optional) require user-defined top-level names
;;; to override same-named primitives, which the compiler's symbol
;;; resolution does not currently do.
;;;
;;; The `include` directive currently has issues in nested scopes; copy
;;; these definitions directly into your program.
;;; ============================================================================

;; list-copy: shallow copy of a list (R7RS §6.4).
(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

;; list-tabulate: build a list of length n by calling proc on 0..n-1.
;; SRFI 1 style; not strictly R7RS but commonly bundled.
(define (list-tabulate n proc)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons (proc i) acc)))))

;;; End of r7rs-lists.scm
