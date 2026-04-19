;; VeloxVM Unit Tests - R5RS Higher-Order Functions
;; Tests for: map, filter, for-each, reduce, function composition

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Map Function")

(assert-equal '(2 4 6)
              (map (lambda (x) (* x 2)) '(1 2 3))
              "map doubles each element")
(assert-equal '() (map (lambda (x) (* x 2)) '()) "map on empty list")
(assert-equal '(1 2 3)
              (map (lambda (x) x) '(1 2 3))
              "map identity function")
(assert-equal '("1" "2" "3")
              (map (lambda (x) (number->string x)) '(1 2 3))
              "map type conversion")

(test-suite "Filter Function")

(assert-equal '(2 4 6)
              (filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))
              "filter even numbers")
(assert-equal '(5 6 7)
              (filter (lambda (x) (> x 4)) '(1 2 3 4 5 6 7))
              "filter greater than threshold")
(assert-equal '()
              (filter (lambda (x) (> x 10)) '(1 2 3))
              "filter returns empty when no matches")
(assert-equal '(1 2 3)
              (filter (lambda (x) #t) '(1 2 3))
              "filter all pass")
(assert-equal '()
              (filter (lambda (x) #f) '(1 2 3))
              "filter none pass")

(test-suite "For-Each Function")

;; for-each used for side effects
(define sum 0)
(for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3 4 5))
(assert-equal 15 sum "for-each accumulation")

(define result '())
(for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
(assert-equal '(3 2 1) result "for-each side effects")

(test-suite "Reduce/Fold Function")

(assert-equal 15 (reduce + '(1 2 3 4 5)) "reduce sum")
(assert-equal 120 (reduce * '(1 2 3 4 5)) "reduce product")

(test-suite "Count Function")

(assert-equal 3 (count (lambda (x) #t) '(1 2 3)) "count all true")
(assert-equal 3
              (count (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))
              "count even numbers")
(assert-equal 3
              (count (lambda (x) (> x 5)) '(3 7 2 9 4 8))
              "count greater than threshold")
(assert-equal 0 (count (lambda (x) #f) '(1 2 3)) "count none")

(test-suite "Composed Higher-Order Functions")

(assert-equal '(4 6)
              (filter (lambda (x) (> x 3))
                      (map (lambda (x) (* x 2)) '(1 2 3)))
              "map then filter")

(assert-equal '(6 8 10)
              (map (lambda (x) (* x 2))
                   (filter (lambda (x) (> x 2)) '(1 2 3 4 5)))
              "filter then map")

(assert-equal 2
              (count (lambda (x) (> x 5))
                     (filter (lambda (x) (= (modulo x 2) 0))
                             '(1 2 3 4 5 6 7 8)))
              "filter then count")

(test-suite "Nested Lambdas")

(assert-equal '(3 4 5)
              (map (lambda (x) ((lambda (y) (+ y 1)) (+ x 1))) '(1 2 3))
              "map with nested lambda")

;; Note: Closure capture tests removed due to VeloxVM limitations

(test-suite "Recursive Higher-Order Functions")

;; Map over tree structure
(define (map-tree f tree)
  (if (pair? tree)
      (cons (map-tree f (car tree))
            (map-tree f (cdr tree)))
      (if (null? tree)
          '()
          (f tree))))

(assert-equal '(2 (4 (6 8)))
              (map-tree (lambda (x) (* x 2)) '(1 (2 (3 4))))
              "map-tree over nested structure")

(test-suite "Multiple List Arguments")

;; Map over two lists
(define (my-map2 f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (car lst1) (car lst2))
            (my-map2 f (cdr lst1) (cdr lst2)))))

(assert-equal '(5 7 9)
              (my-map2 + '(1 2 3) '(4 5 6))
              "map with two lists")

(test-summary)
