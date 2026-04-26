;; VeloxVM Unit Tests - Box Operations
;; Tests for: box, box-ref, box-set!
;;
;; Boxes are 1-cell heap containers used by the compiler to give
;; closure-captured-and-mutated variables shared mutable storage.

(include "../unit-test-framework.scm")

(test-suite "Box Operations")

;; Basic create + read.
(define b1 (box 42))
(assert-equal 42 (box-ref b1) "box-ref returns initial value")

;; box-set! mutates.
(box-set! b1 99)
(assert-equal 99 (box-ref b1) "box-ref returns value after box-set!")

;; box accepts any object type.
(define b-str (box "hello"))
(assert-equal "hello" (box-ref b-str) "box of string round-trips")

(define b-list (box '(1 2 3)))
(assert-equal '(1 2 3) (box-ref b-list) "box of list round-trips")

(define b-char (box #\A))
(assert-equal #\A (box-ref b-char) "box of char round-trips")

;; box-set! can replace with a different type.
(define b-poly (box 1))
(box-set! b-poly "now a string")
(assert-equal "now a string" (box-ref b-poly) "box-set! replaces with different type")

;; Mutation through a function argument is visible to the caller --
;; this is what makes shared mutable state through closures work later.
(define (mutator b new-val) (box-set! b new-val))
(define b-shared (box 0))
(mutator b-shared 100)
(assert-equal 100 (box-ref b-shared) "box-set! through function arg is visible to caller")

(test-summary)
