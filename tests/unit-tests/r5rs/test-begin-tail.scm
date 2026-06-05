;;; VeloxVM Unit Tests - begin tail-position semantics (R5RS 4.2.3 / 3.5)
;;;
;;; Regression for the begin tail-flag bug in core/expr-primitives.c: begin
;;; used to mark ALL of its sub-expressions as tail calls, not just the
;;; last. A non-last recursive call inside a begin was therefore wrongly
;;; tail-folded (its frame reused) and never actually recursed, so the
;;; expressions after it ran far too few times. Only the FINAL expression
;;; of a begin is in tail position; the leading ones run for effect.
;;;
;;; Depths are kept small so the genuine (non-tail) recursion fits the
;;; VM context stack; the point is correctness, not depth.

(include "../unit-test-framework.scm")

(test-suite "begin: only the last expression is in tail position")

;; In (begin (recurse) (bump!)) the recursion must run to completion, so
;; bump! fires once per level on the way back up. Before the fix the
;; recursive call was tail-folded and bump! fired exactly once.
(define counter (make-vector 1 0))
(define (descend n)
  (when (> n 0)
    (begin (descend (- n 1))                                  ; non-tail
           (vector-set! counter 0 (+ 1 (vector-ref counter 0)))))) ; tail
(descend 12)
(assert-equal 12 (vector-ref counter 0)
              "leading recursive call in begin runs to completion")

;; Three expressions: the first is a non-tail recursive call, the middle
;; and last run for effect. Each level adds 2, so g(10) yields 20.
(define c2 (make-vector 1 0))
(define (g n)
  (when (> n 0)
    (begin (g (- n 1))
           (vector-set! c2 0 (+ 1 (vector-ref c2 0)))
           (vector-set! c2 0 (+ 1 (vector-ref c2 0))))))
(g 10)
(assert-equal 20 (vector-ref c2 0)
              "all leading expressions of a begin run, not just the last")

;; Tail recursion through begin's LAST expression must still fold (no
;; overflow at depth far beyond the context stack).
(define (countdown n)
  (when (> n 0) (begin 1 (countdown (- n 1)))))
(countdown 200000)
(assert-true #t "tail call in begin's last expression still folds (no overflow)")

(test-summary)
