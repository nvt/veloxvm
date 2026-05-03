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

(define (exact-integer-sqrt n)
  (if (< n 2)
      (list n 0)
      (let loop ((x n))
        (let ((y (quotient (+ x (quotient n x)) 2)))
          (if (>= y x)
              (list x (- n (* x x)))
              (loop y))))))

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

;; exact-integer-sqrt: returns (s r) with s = floor(sqrt(n)), r = n - s*s
(assert-equal '(0 0)  (exact-integer-sqrt 0)    "exact-integer-sqrt of 0")
(assert-equal '(1 0)  (exact-integer-sqrt 1)    "exact-integer-sqrt of 1")
(assert-equal '(1 1)  (exact-integer-sqrt 2)    "exact-integer-sqrt of 2")
(assert-equal '(1 2)  (exact-integer-sqrt 3)    "exact-integer-sqrt of 3")
(assert-equal '(2 0)  (exact-integer-sqrt 4)    "exact-integer-sqrt of perfect 4")
(assert-equal '(3 1)  (exact-integer-sqrt 10)   "exact-integer-sqrt of 10")
(assert-equal '(10 0) (exact-integer-sqrt 100)  "exact-integer-sqrt of perfect 100")
(assert-equal '(31 39) (exact-integer-sqrt 1000) "exact-integer-sqrt of 1000")

(test-summary)
