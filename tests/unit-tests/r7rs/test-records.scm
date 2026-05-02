;;; VeloxVM Unit Tests - R7RS records (define-record-type)
;;; Tests for the rewriter expansion of (define-record-type ...) into
;;; tag-symbol + constructor + predicate + accessor [+ mutator].

(include "../unit-test-framework.scm")

(test-suite "R7RS records (define-record-type)")

;; Basic case: two fields, one with a mutator
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y set-point-y!))

(define p (make-point 3 4))

(assert-equal #t (point? p) "predicate matches its own record")
(assert-equal #f (point? 42) "predicate rejects integer")
(assert-equal #f (point? '(1 2 3)) "predicate rejects list")
(assert-equal #f (point? "point") "predicate rejects string")
(assert-equal #f (point? #f) "predicate rejects #f")

(assert-equal 3 (point-x p) "first-field accessor")
(assert-equal 4 (point-y p) "second-field accessor")

(set-point-y! p 99)
(assert-equal 99 (point-y p) "mutator changes the field")
(assert-equal 3 (point-x p) "mutator does not affect other fields")

;; Type isolation: a different record type's predicate must reject
(define-record-type circle
  (make-circle radius)
  circle?
  (radius circle-radius))

(define c (make-circle 7))

(assert-equal #t (circle? c) "circle? matches circle")
(assert-equal #f (circle? p) "circle? rejects point")
(assert-equal #f (point? c) "point? rejects circle")
(assert-equal 7 (circle-radius c) "circle accessor")

;; Multiple instances of the same type are distinct vectors
(define p1 (make-point 1 2))
(define p2 (make-point 1 2))
(assert-equal 1 (point-x p1) "instance 1 field x")
(assert-equal 1 (point-x p2) "instance 2 field x")
(set-point-y! p1 555)
(assert-equal 555 (point-y p1) "mutating p1 sees the change")
(assert-equal 2 (point-y p2) "mutating p1 does not affect p2")

;; A record with no mutators (read-only)
(define-record-type pair-rec
  (make-pair-rec head tail)
  pair-rec?
  (head pair-head)
  (tail pair-tail))

(define pr (make-pair-rec 'a 'b))
(assert-equal #t (pair-rec? pr) "pair-rec predicate")
(assert-equal 'a (pair-head pr) "pair-rec head")
(assert-equal 'b (pair-tail pr) "pair-rec tail")

;; Constructor with subset of fields: uninitialized fields default to #f
(define-record-type sparse
  (make-sparse a)
  sparse?
  (a sparse-a)
  (b sparse-b set-sparse-b!))

(define s (make-sparse 10))
(assert-equal 10 (sparse-a s) "constructor sets the named field")
(assert-equal #f (sparse-b s) "field not in constructor defaults to #f")
(set-sparse-b! s 20)
(assert-equal 20 (sparse-b s) "mutator initializes the unset field")

(test-summary)
