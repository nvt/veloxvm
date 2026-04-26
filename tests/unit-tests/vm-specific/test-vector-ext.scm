;; VeloxVM Unit Tests - Vector Extensions (VM-Specific)
;; Tests for VeloxVM-specific vector procedures: vector->string, vector-merge,
;; vector-for-each, vector-count, vector-fold, vector-map.
;; Note: R5RS provides standard vector procedures. These are VeloxVM extensions.

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Vector Extensions (VM-Specific)")

;; Vector->string (VM-specific: not in R5RS)
;; Converts a vector of characters to a string
(assert-equal "abc" (vector->string '#(#\a #\b #\c)) "Vector of chars to string")

;; Vector-merge (VM-specific: not in R5RS)
;; Concatenates multiple vectors into one
(assert-equal '#(1 2 3 4 5 6) (vector-merge '#(1 2 3) '#(4 5 6)) "Merge two vectors")
(assert-equal '#(1 2 3) (vector-merge '#(1 2 3) '#()) "Merge with empty vector")
(assert-equal '#(4 5 6) (vector-merge '#() '#(4 5 6)) "Merge empty with vector")
(assert-equal '#(1 2 3 4 5 6 7 8 9) (vector-merge '#(1 2 3) '#(4 5 6) '#(7 8 9)) "Merge three vectors")

;; vector-for-each — side-effecting iteration
(define vfe-sum 0)
(vector-for-each (lambda (x) (set! vfe-sum (+ vfe-sum x))) '#(1 2 3 4 5))
(assert-equal 15 vfe-sum "vector-for-each accumulation")

(define vfe-empty 99)
(vector-for-each (lambda (x) (set! vfe-empty 0)) '#())
(assert-equal 99 vfe-empty "vector-for-each empty leaves state unchanged")

;; vector-count — count elements matching a predicate
(assert-equal 4 (vector-count (lambda (x) (zero? (modulo x 2)))
                              '#(10 20 30 40))
              "vector-count all match")
(assert-equal 0 (vector-count (lambda (x) (> x 100)) '#(1 2 3))
              "vector-count none match")
(assert-equal 0 (vector-count (lambda (x) #t) '#())
              "vector-count empty vector")

;; vector-fold — left fold with explicit initial accumulator
(assert-equal 100 (vector-fold (lambda (acc x) (+ acc x)) 0 '#(10 20 30 40))
              "vector-fold sum")
(assert-equal 99 (vector-fold (lambda (acc x) (+ acc x)) 99 '#())
              "vector-fold empty returns init")
(assert-equal 24 (vector-fold (lambda (acc x) (* acc x)) 1 '#(2 3 4))
              "vector-fold product")

;; vector-map — element-wise transform, returns a new vector
(assert-equal '#(100 400 900 1600)
              (vector-map (lambda (x) (* x x)) '#(10 20 30 40))
              "vector-map squares")
(assert-equal '#()
              (vector-map (lambda (x) x) '#())
              "vector-map empty vector")
(assert-equal '#(2 3 4)
              (vector-map (lambda (x) (+ x 1)) '#(1 2 3))
              "vector-map increment")

(test-summary)
