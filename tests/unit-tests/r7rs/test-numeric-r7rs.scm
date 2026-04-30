;;; VeloxVM Unit Tests - R7RS numeric helpers
;;; Tests for: square, exact-integer?, truncate-quotient, truncate-remainder,
;;;            floor-quotient, floor-remainder.

(include "../unit-test-framework.scm")

;; Inline definitions from languages/scheme-racket/runtime/r7rs-numeric.scm
(define (square x) (* x x))

(define (exact-integer? x)
  (and (integer? x) (exact? x)))

(define (truncate-quotient n m) (quotient n m))
(define (truncate-remainder n m) (remainder n m))

(define (floor-quotient n m)
  (let ((q (quotient n m))
        (r (remainder n m)))
    (if (and (not (= r 0))
             (or (and (< n 0) (> m 0))
                 (and (> n 0) (< m 0))))
        (- q 1)
        q)))

(define (floor-remainder n m) (modulo n m))

(test-suite "R7RS numeric helpers")

;; square
(assert-equal 0  (square 0)  "square of 0")
(assert-equal 1  (square 1)  "square of 1")
(assert-equal 9  (square 3)  "square of 3")
(assert-equal 9  (square -3) "square of -3")
(assert-equal 1/4 (square 1/2) "square of rational")

;; exact-integer?
(assert-equal #t (exact-integer? 0)   "0 is an exact integer")
(assert-equal #t (exact-integer? 42)  "42 is an exact integer")
(assert-equal #t (exact-integer? -7)  "-7 is an exact integer")
(assert-equal #f (exact-integer? 1/2) "1/2 is not an integer")
(assert-equal #f (exact-integer? "a") "non-number is not an exact integer")

;; truncate-quotient and truncate-remainder match R5RS quotient/remainder
(assert-equal 3  (truncate-quotient 10 3)   "truncate-quotient 10/3")
(assert-equal -3 (truncate-quotient 10 -3)  "truncate-quotient 10/-3")
(assert-equal -3 (truncate-quotient -10 3)  "truncate-quotient -10/3")
(assert-equal 3  (truncate-quotient -10 -3) "truncate-quotient -10/-3")

(assert-equal 1  (truncate-remainder 10 3)   "truncate-remainder 10/3")
(assert-equal 1  (truncate-remainder 10 -3)  "truncate-remainder 10/-3")
(assert-equal -1 (truncate-remainder -10 3)  "truncate-remainder -10/3")
(assert-equal -1 (truncate-remainder -10 -3) "truncate-remainder -10/-3")

;; floor-quotient: rounds toward -infinity
(assert-equal 3  (floor-quotient 10 3)   "floor-quotient 10/3 (same signs)")
(assert-equal -4 (floor-quotient 10 -3)  "floor-quotient 10/-3 (signs differ)")
(assert-equal -4 (floor-quotient -10 3)  "floor-quotient -10/3 (signs differ)")
(assert-equal 3  (floor-quotient -10 -3) "floor-quotient -10/-3 (same signs)")
(assert-equal 5  (floor-quotient 25 5)   "floor-quotient with exact division")
(assert-equal -5 (floor-quotient 25 -5)  "floor-quotient exact, signs differ")
(assert-equal 0  (floor-quotient 0 5)    "floor-quotient of zero")

;; floor-remainder matches modulo
(assert-equal 1  (floor-remainder 10 3)   "floor-remainder 10/3")
(assert-equal -2 (floor-remainder 10 -3)  "floor-remainder 10/-3")
(assert-equal 2  (floor-remainder -10 3)  "floor-remainder -10/3")
(assert-equal -1 (floor-remainder -10 -3) "floor-remainder -10/-3")
(assert-equal 0  (floor-remainder 25 5)   "floor-remainder when exact")

(test-summary)
