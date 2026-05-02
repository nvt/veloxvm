;;; VeloxVM Unit Tests - R7RS case-lambda
;;; Tests the rewriter expansion of (case-lambda ...) into a single
;;; lambda that dispatches on (length args).

(include "../unit-test-framework.scm")

(test-suite "R7RS case-lambda")

;; Three exact-arity clauses
(define triad
  (case-lambda
    (() 'zero)
    ((x) (list 'one x))
    ((x y) (list 'two x y))))

(assert-equal 'zero        (triad)        "0-arg clause matches")
(assert-equal '(one 10)    (triad 10)     "1-arg clause matches")
(assert-equal '(two 10 20) (triad 10 20)  "2-arg clause matches")

;; First-match wins: an earlier clause shadows a later one when arity matches
(define ambiguous
  (case-lambda
    ((x) (list 'first x))
    ((x) (list 'second x))))

(assert-equal '(first 5) (ambiguous 5)
              "first matching clause wins")

;; Bare-symbol catch-all clause
(define collect
  (case-lambda
    ((x) (list 'one x))
    (rest (cons 'many rest))))

(assert-equal '(one 7)        (collect 7)       "1-arg routes to first clause")
(assert-equal '(many)         (collect)         "0-args routes to catch-all")
(assert-equal '(many 1 2 3)   (collect 1 2 3)   "3-args routes to catch-all")
(assert-equal '(many 1 2 3 4) (collect 1 2 3 4) "4-args routes to catch-all")

;; Dotted-pair formals: at least N fixed args, rest gets the tail
(define head-and-tail
  (case-lambda
    ((a) (list 'just a))
    ((a . rest) (cons a rest))))

(assert-equal '(just 1)    (head-and-tail 1)        "1-arg matches first clause")
(assert-equal '(1 2)       (head-and-tail 1 2)      "2-arg routes to dotted clause")
(assert-equal '(1 2 3 4)   (head-and-tail 1 2 3 4)  "4-arg routes to dotted clause")

;; Two fixed + rest
(define two-fixed-rest
  (case-lambda
    ((a b) (list 'pair a b))
    ((a b . rest) (list 'triple-or-more a b rest))))

(assert-equal '(pair 1 2)
              (two-fixed-rest 1 2)
              "exactly 2 args matches first clause")
(assert-equal '(triple-or-more 1 2 (3))
              (two-fixed-rest 1 2 3)
              "3 args routes to dotted clause")
(assert-equal '(triple-or-more 1 2 (3 4 5))
              (two-fixed-rest 1 2 3 4 5)
              "5 args routes to dotted clause with rest list")

;; Single clause: degenerate but legal
(define only-zero
  (case-lambda
    (() 'success)))

(assert-equal 'success (only-zero) "single clause works")

;; Catch-all-only: equivalent to (lambda args body)
(define catch-all
  (case-lambda
    (xs (length xs))))

(assert-equal 0 (catch-all)         "catch-all 0 args")
(assert-equal 3 (catch-all 1 2 3)   "catch-all 3 args")

(test-summary)
