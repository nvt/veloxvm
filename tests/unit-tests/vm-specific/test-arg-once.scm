;; VeloxVM Unit Tests - Single argument evaluation
;;
;; Regression coverage for argument-duplicating rewriters in the Racket
;; compiler. abs, max, min, and the level-2 (* x 2) strength reduction
;; previously substituted the argument expression directly into the
;; output template, which silently re-evaluated the expression at every
;; substitution site -- a correctness bug for side-effecting arguments
;; and wasted work for pure ones. They now bind the argument to a
;; gensym temp first.

(include "../unit-test-framework.scm")

(test-suite "Argument-once evaluation")

;; ============================================================================
;; abs evaluates its argument exactly once
;; ============================================================================

(define abs-counter 0)
(define (count-and-return-neg)
  (set! abs-counter (+ abs-counter 1))
  -7)

(define abs-result (abs (count-and-return-neg)))
(assert-equal 7 abs-result "abs of -7 -> 7")
(assert-equal 1 abs-counter "abs evaluated its argument exactly once")

;; Positive path: same single-eval guarantee.
(set! abs-counter 0)
(define (count-and-return-pos)
  (set! abs-counter (+ abs-counter 1))
  5)

(define abs-pos-result (abs (count-and-return-pos)))
(assert-equal 5 abs-pos-result "abs of 5 -> 5")
(assert-equal 1 abs-counter "abs evaluates positive arg exactly once")

;; ============================================================================
;; max evaluates each argument exactly once
;; ============================================================================

(define max-a-count 0)
(define max-b-count 0)
(define (count-a-3)
  (set! max-a-count (+ max-a-count 1))
  3)
(define (count-b-9)
  (set! max-b-count (+ max-b-count 1))
  9)

(define max-result (max (count-a-3) (count-b-9)))
(assert-equal 9 max-result "max of 3 and 9 -> 9")
(assert-equal 1 max-a-count "max evaluates first arg exactly once")
(assert-equal 1 max-b-count "max evaluates second arg exactly once")

;; ============================================================================
;; min evaluates each argument exactly once
;; ============================================================================

(define min-a-count 0)
(define min-b-count 0)
(define (count-a-4)
  (set! min-a-count (+ min-a-count 1))
  4)
(define (count-b-2)
  (set! min-b-count (+ min-b-count 1))
  2)

(define min-result (min (count-a-4) (count-b-2)))
(assert-equal 2 min-result "min of 4 and 2 -> 2")
(assert-equal 1 min-a-count "min evaluates first arg exactly once")
(assert-equal 1 min-b-count "min evaluates second arg exactly once")

;; ============================================================================
;; Pure-value paths still produce correct results
;; ============================================================================

(assert-equal 5  (abs -5)      "abs of literal -5")
(assert-equal 5  (abs 5)       "abs of literal 5")
(assert-equal 0  (abs 0)       "abs of zero")
(assert-equal 9  (max 3 9)     "max of literals")
(assert-equal 9  (max 9 3)     "max of literals (other order)")
(assert-equal 2  (min 4 2)     "min of literals")
(assert-equal 2  (min 2 4)     "min of literals (other order)")

(test-summary)
