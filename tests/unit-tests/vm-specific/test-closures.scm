;; VeloxVM Unit Tests - Closures (immutable capture)
;;
;; Tests for closure capture of outer-lambda parameters that are read
;; but not mutated. set!-on-captured patterns (make-counter style)
;; need the box rewrite from phase 5 and are not exercised here.

(include "../unit-test-framework.scm")

(test-suite "Closures (immutable capture)")

;; make-adder: classic capture-by-value pattern.
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(assert-equal 8 (add5 3) "make-adder: 5 + 3 = 8")
(assert-equal 12 (add5 7) "make-adder: 5 + 7 = 12")
(assert-equal 13 (add10 3) "make-adder: 10 + 3 = 13")
(assert-equal 0 (add10 -10) "make-adder with negative arg")

;; Two distinct closures over different captured values don't interfere.
(assert-equal 8 (add5 3) "add5 still works after add10 calls")

;; Multiple captures in one closure.
(define (make-mixer a b)
  (lambda (x) (+ (* a x) b)))

(define line2x+3 (make-mixer 2 3))
(define line5x-1 (make-mixer 5 -1))

(assert-equal 7 (line2x+3 2) "2*2 + 3 = 7")
(assert-equal 13 (line2x+3 5) "2*5 + 3 = 13")
(assert-equal 14 (line5x-1 3) "5*3 - 1 = 14")

;; Closure captures a list (heap value).
(define (make-prepender prefix)
  (lambda (lst) (cons prefix lst)))

(define add-zero (make-prepender 0))
(assert-equal '(0 1 2) (add-zero '(1 2)) "closure captures list value")

;; Triple-nested lambda: outer captures from outermost.
(define (make-curried-add a)
  (lambda (b)
    (lambda (c) (+ a b c))))

(define add-from-1 (make-curried-add 1))
(define add-1-2 (add-from-1 2))
(assert-equal 6 (add-1-2 3) "curried 1 + 2 + 3 = 6")
(assert-equal 13 (add-1-2 10) "curried 1 + 2 + 10 = 13")

(test-summary)
