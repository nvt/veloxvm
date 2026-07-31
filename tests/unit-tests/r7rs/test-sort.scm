;;; VeloxVM Unit Tests - stable sorting from the (velox sort) library
;;; (list-sort, vector-sort, vector-sort!). Verifies correctness across
;;; sizes, stability of equal keys, and the destructive vs non-destructive
;;; vector forms. Also exercises importing a shipped library by name via
;;; the compiler's runtime search path.

(include "../unit-test-framework.scm")
;; Importable sort library, found on the compiler runtime search path.
(import (velox sort))

(test-suite "stable sorting (velox sort)")

;; --- list-sort: edges and ordering ----------------------------------
(assert-equal '()            (list-sort < '())            "list-sort empty")
(assert-equal '(1)           (list-sort < '(1))           "list-sort singleton")
(assert-equal '(1 2)         (list-sort < '(2 1))         "list-sort pair")
(assert-equal '(1 2 3 4 5)   (list-sort < '(1 2 3 4 5))   "list-sort already sorted")
(assert-equal '(1 2 3 4 5)   (list-sort < '(5 4 3 2 1))   "list-sort reversed")
(assert-equal '(1 1 2 3 3 4 5 5)
              (list-sort < '(3 1 4 1 5 5 2 3))
              "list-sort with duplicates")
(assert-equal '(-3 -1 0 2 7)
              (list-sort < '(2 -1 7 0 -3))
              "list-sort with negatives")

;; A mid-size shuffle (kept to 15 elements so the quoted literal stays
;; within the 16-slot argv frame; larger inputs are built below).
(assert-equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
              (list-sort < '(11 3 14 0 7 9 2 5 12 1 8 4 13 6 10))
              "list-sort 15 elements")

;; Descending via a flipped comparator.
(assert-equal '(5 4 3 2 1)
              (list-sort > '(3 1 4 5 2))
              "list-sort descending comparator")

;; --- stability ------------------------------------------------------
;; Sort (key . tag) pairs by key only; equal keys must keep input order.
;; (Built with cons rather than dotted-pair literals, which the compiler
;; does not accept inside a quote.)
(define (by-key a b) (< (car a) (car b)))
(define unsorted-pairs
  (list (cons 2 'd) (cons 1 'a) (cons 1 'b) (cons 2 'e) (cons 1 'c)))
(define expected-pairs
  (list (cons 1 'a) (cons 1 'b) (cons 1 'c) (cons 2 'd) (cons 2 'e)))
(assert-equal expected-pairs
              (list-sort by-key unsorted-pairs)
              "list-sort is stable on equal keys")

;; --- vector-sort (non-destructive) ----------------------------------
(define v (vector 3 1 2))
(assert-equal (vector 1 2 3) (vector-sort < v) "vector-sort returns sorted copy")
(assert-equal (vector 3 1 2) v                 "vector-sort leaves original intact")

;; --- vector-sort! (in place) ----------------------------------------
(define w (vector 5 3 8 1 9 2 7 4 6 0))
(vector-sort! < w)
(assert-equal (vector 0 1 2 3 4 5 6 7 8 9) w "vector-sort! sorts in place")

(define w1 (vector 42))
(vector-sort! < w1)
(assert-equal (vector 42) w1 "vector-sort! singleton")

;; --- larger vector, built programmatically (worst case: reversed) ----
;; Confirms n > 16 works (beyond the argv-frame literal limit) and that
;; the in-place sort holds up on a fully reversed input.
(define (sorted? v)
  (let ((n (vector-length v)))
    (let loop ((i 1))
      (cond ((>= i n) #t)
            ((< (vector-ref v i) (vector-ref v (- i 1))) #f)
            (else (loop (+ i 1)))))))
(define big (make-vector 50 0))
(let fill ((i 0))
  (when (< i 50)
    (vector-set! big i (- 50 i))    ; 50, 49, ..., 1  (reverse sorted)
    (fill (+ i 1))))
(vector-sort! < big)
(assert-true  (sorted? big)             "vector-sort! 50 reversed -> sorted")
(assert-equal 1  (vector-ref big 0)     "vector-sort! 50: min at front")
(assert-equal 50 (vector-ref big 49)    "vector-sort! 50: max at back")

(test-summary)
