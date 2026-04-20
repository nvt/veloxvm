;; VeloxVM Unit Tests - Vector Operations (R5RS-Compliant)
;; Tests for R5RS vector procedures: make-vector, vector, vector?, vector-length,
;; vector-ref, vector-set!, vector->list, list->vector, vector-fill!

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Vector Operations - Basic (R5RS)")

;; Make-vector (R5RS §6.3.6)
(assert-equal 5 (vector-length (make-vector 5)) "Make-vector creates correct length")
(assert-equal '#(0 0 0 0 0) (make-vector 5 0) "Make-vector with fill value")
(assert-equal 0 (vector-length (make-vector 0)) "Make-vector with zero length")

;; Vector (from elements) (R5RS §6.3.6)
(assert-equal '#(1 2 3) (vector 1 2 3) "Vector from elements")
(assert-equal '#(a b c) (vector 'a 'b 'c) "Vector of symbols")
(assert-equal '#("hello" "world") (vector "hello" "world") "Vector of strings")
(assert-equal '#((1 2) (3 4)) (vector '(1 2) '(3 4)) "Vector of lists")

;; Vector? (R5RS §6.3.6)
(assert-true (vector? '#(1 2 3)) "Vector is a vector")
(assert-true (vector? (make-vector 5)) "Make-vector result is a vector")
(assert-false (vector? '(1 2 3)) "List is not a vector")
(assert-false (vector? "hello") "String is not a vector")
(assert-false (vector? 123) "Number is not a vector")

;; Vector-length (R5RS §6.3.6)
(assert-equal 0 (vector-length '#()) "Length of empty vector")
(assert-equal 3 (vector-length '#(1 2 3)) "Length of 3-element vector")
(assert-equal 5 (vector-length (make-vector 5)) "Length of make-vector result")

;; Vector-ref (R5RS §6.3.6)
(assert-equal 1 (vector-ref '#(1 2 3 4 5) 0) "First element")
(assert-equal 3 (vector-ref '#(1 2 3 4 5) 2) "Middle element")
(assert-equal 5 (vector-ref '#(1 2 3 4 5) 4) "Last element")
(assert-equal 'b (vector-ref '#(a b c) 1) "Vector-ref with symbols")

;; Vector-set! (R5RS §6.3.6)
(define test-vec (make-vector 3 0))
(vector-set! test-vec 0 10)
(assert-equal 10 (vector-ref test-vec 0) "Vector-set! modifies element")
(vector-set! test-vec 1 20)
(vector-set! test-vec 2 30)
(assert-equal '#(10 20 30) test-vec "Vector-set! all elements")

(test-suite "Vector Operations - Conversion (R5RS)")

;; Vector->list (R5RS §6.3.6)
(assert-equal '(1 2 3) (vector->list '#(1 2 3)) "Vector to list")
(assert-equal '() (vector->list '#()) "Empty vector to list")
(assert-equal '(a b c) (vector->list '#(a b c)) "Vector of symbols to list")

;; List->vector (R5RS §6.3.6)
(assert-equal '#(1 2 3) (list->vector '(1 2 3)) "List to vector")
(assert-equal '#() (list->vector '()) "Empty list to vector")
(assert-equal '#(a b c) (list->vector '(a b c)) "List of symbols to vector")

;; Round trip
(assert-equal '(1 2 3) (vector->list (list->vector '(1 2 3))) "Round trip list->vector->list")
(assert-equal '#(1 2 3) (list->vector (vector->list '#(1 2 3))) "Round trip vector->list->vector")

(test-suite "Vector Operations - Modification (R5RS)")

;; Vector-fill! (R5RS §6.3.6)
(define fill-vec (make-vector 5 0))
(vector-fill! fill-vec 42)
(assert-equal '#(42 42 42 42 42) fill-vec "Vector-fill! fills all elements")

(test-suite "Vector Operations - Do Loop Pattern (R5RS)")

;; Using do loop to fill vector (common pattern)
(define do-vec
  (do ((vec (make-vector 5))
       (i 0 (+ i 1)))
      ((= i 5) vec)
    (vector-set! vec i i)))
(assert-equal '#(0 1 2 3 4) do-vec "Do loop fills vector with indices")

;; Squares using do loop
(define squares-vec
  (do ((vec (make-vector 5))
       (i 0 (+ i 1)))
      ((= i 5) vec)
    (vector-set! vec i (* i i))))
(assert-equal '#(0 1 4 9 16) squares-vec "Do loop fills vector with squares")

(test-suite "Vector Operations - For-each Pattern (R5RS)")

;; Using for-each to modify vector
(define for-vec (make-vector 5))
(for-each (lambda (i) (vector-set! for-vec i (* i i))) '(0 1 2 3 4))
(assert-equal '#(0 1 4 9 16) for-vec "For-each fills vector with squares")

(test-suite "Vector Operations - Nested Vectors (R5RS)")

;; Nested vectors
(define nested '#(#(1 2) #(3 4) #(5 6)))
(assert-equal '#(1 2) (vector-ref nested 0) "Access nested vector")
(assert-equal 3 (vector-ref (vector-ref nested 1) 0) "Access element of nested vector")

;; 2D vector simulation
(define (make-2d-vector rows cols init)
  (let ((v (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((= i rows) v)
      (vector-set! v i (make-vector cols init)))))

(define matrix (make-2d-vector 3 3 0))
(vector-set! (vector-ref matrix 1) 1 5)
(assert-equal 5 (vector-ref (vector-ref matrix 1) 1) "2D vector access and modify")

(test-suite "Vector Operations - Mixed Types (R5RS)")

;; Vector with mixed types
(define mixed '#(1 "two" #\3 (4 5) #(6 7)))
(assert-equal 1 (vector-ref mixed 0) "Integer in mixed vector")
(assert-equal "two" (vector-ref mixed 1) "String in mixed vector")
(assert-equal #\3 (vector-ref mixed 2) "Char in mixed vector")
(assert-equal '(4 5) (vector-ref mixed 3) "List in mixed vector")
(assert-equal '#(6 7) (vector-ref mixed 4) "Vector in mixed vector")

(test-suite "Vector Operations - Equality (R5RS)")

;; Vector equality
(assert-false (eqv? (vector 1 2 3) (vector 1 2 3)) "Different vector objects not eqv?")
(assert-equal '#(1 2 3) (vector 1 2 3) "Vectors with same content equal?")
(assert-true (equal? '#(1 2 #(3 4)) '#(1 2 #(3 4))) "Nested vectors structural equality")
(assert-false (equal? '#(1 2 3) '#(1 2 4)) "Different vectors not equal")

(test-summary)
