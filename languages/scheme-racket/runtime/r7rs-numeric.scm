;;; ============================================================================
;;; VeloxVM R7RS Numeric Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small numeric helpers that aren't VM primitives. Pure Scheme on
;;; top of quotient, remainder, modulo, integer?, exact?.
;;;
;;; The `include` directive currently has issues in nested scopes; copy
;;; these definitions directly into your program.
;;; ============================================================================

;; square: (square x) => (* x x). R7RS §6.2.6.
(define (square x) (* x x))

;; exact-integer?: True iff x is both an integer and exact. R7RS §6.2.6.
(define (exact-integer? x)
  (and (integer? x) (exact? x)))

;; truncate-quotient / truncate-remainder: signs-of-the-results match
;; R5RS quotient and remainder, which already truncate toward zero.
(define (truncate-quotient n m) (quotient n m))
(define (truncate-remainder n m) (remainder n m))

;; floor-quotient / floor-remainder: round the quotient toward negative
;; infinity. floor-remainder coincides with R5RS modulo. floor-quotient
;; is one less than truncate-quotient when the operands have differing
;; signs and the remainder is non-zero.
(define (floor-quotient n m)
  (let ((q (quotient n m))
        (r (remainder n m)))
    (if (and (not (= r 0))
             (or (and (< n 0) (> m 0))
                 (and (> n 0) (< m 0))))
        (- q 1)
        q)))

(define (floor-remainder n m) (modulo n m))

;; exact-integer-sqrt: returns (s r) where s = floor(sqrt(n)) and
;; r = n - s*s, for non-negative integer n. R7RS §6.2.6 specifies two
;; return values; this VM has no multiple-value return, so the result
;; is packed as a 2-element list. Newton's method on integers; converges
;; in O(log n) iterations.
(define (exact-integer-sqrt n)
  (if (< n 2)
      (list n 0)
      (let loop ((x n))
        (let ((y (quotient (+ x (quotient n x)) 2)))
          (if (>= y x)
              (list x (- n (* x x)))
              (loop y))))))

;;; End of r7rs-numeric.scm
