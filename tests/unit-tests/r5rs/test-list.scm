;; VeloxVM Unit Tests - List Operations
;; Tests for: list, cons, car, cdr, list-ref, list-tail, append, reverse,
;;            length, null?, list?, pair?, set-car!, set-cdr!,
;;            memq, memv, member, assq, assv, assoc, push, pop, remove

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "List Operations")

;; List construction
(assert-equal '() (list) "Empty list")
(assert-equal '(1) (list 1) "Single element list")
(assert-equal '(1 2 3) (list 1 2 3) "Multiple element list")
(assert-equal '(1 (2 3) 4) (list 1 (list 2 3) 4) "Nested list")

;; Cons
(assert-equal '(1 2 3) (cons 1 '(2 3)) "Cons to front of list")
(assert-equal '(1) (cons 1 '()) "Cons to empty list")
(assert-equal '((1 2) 3) (cons '(1 2) '(3)) "Cons list to list")

;; Car and Cdr
(assert-equal 1 (car '(1 2 3)) "Car of list")
(assert-equal '(2 3) (cdr '(1 2 3)) "Cdr of list")
(assert-equal '() (cdr '(1)) "Cdr of single element list")
(assert-equal '(1 2) (car '((1 2) 3 4)) "Car of nested list")
(assert-equal '(3 4) (cdr '((1 2) 3 4)) "Cdr of nested list")

;; List-ref
(assert-equal 1 (list-ref '(1 2 3 4 5) 0) "First element")
(assert-equal 3 (list-ref '(1 2 3 4 5) 2) "Middle element")
(assert-equal 5 (list-ref '(1 2 3 4 5) 4) "Last element")

;; List-tail
(assert-equal '(3 4 5) (list-tail '(1 2 3 4 5) 2) "Tail from index 2")
(assert-equal '(1 2 3 4 5) (list-tail '(1 2 3 4 5) 0) "Tail from index 0")
(assert-equal '() (list-tail '(1 2 3) 3) "Tail at end")

;; Append
(assert-equal '(1 2 3 4 5 6) (append '(1 2 3) '(4 5 6)) "Append two lists")
(assert-equal '(1 2 3) (append '(1 2 3) '()) "Append empty list")
(assert-equal '(4 5 6) (append '() '(4 5 6)) "Append to empty list")
;; TODO: VM append only handles 2 args - need rewriter for multi-arg
;;(assert-equal '(1 2 3 4 5 6) (append '(1 2) '(3 4) '(5 6)) "Append three lists")

;; Reverse
(assert-equal '(3 2 1) (reverse '(1 2 3)) "Reverse list")
(assert-equal '() (reverse '()) "Reverse empty list")
(assert-equal '(1) (reverse '(1)) "Reverse single element")

;; Remove
(assert-equal '(1 3) (remove 2 '(1 2 3)) "Remove element from list")
(assert-equal '(1 2 3) (remove 4 '(1 2 3)) "Remove non-existent element")
(assert-equal '() (remove 1 '(1)) "Remove only element")

;; Length
(assert-equal 0 (length '()) "Length of empty list")
(assert-equal 1 (length '(1)) "Length of single element list")
(assert-equal 5 (length '(1 2 3 4 5)) "Length of multiple elements")
(assert-equal 3 (length '(1 (2 3) 4)) "Length counts nested list as one")

;; Null?
(assert-true (null? '()) "Empty list is null")
(assert-false (null? '(1)) "Non-empty list is not null")
(assert-false (null? 0) "Zero is not null")

;; List?
(assert-true (list? '()) "Empty list is a list")
(assert-true (list? '(1 2 3)) "List is a list")
(assert-false (list? 5) "Integer is not a list")
(assert-false (list? "hello") "String is not a list")

;; Pair?
(assert-true (pair? (cons 1 2)) "Dotted pair is a pair")
(assert-true (pair? '(1 2 3)) "List is a pair")
(assert-false (pair? '()) "Empty list is not a pair")
(assert-false (pair? 5) "Integer is not a pair")

;; Set-car! and Set-cdr!
(define test-list (list 1 2 3))
(set-car! test-list 10)
(assert-equal '(10 2 3) test-list "Set-car! modifies first element")
(set-cdr! test-list '(20 30))
(assert-equal '(10 20 30) test-list "Set-cdr! modifies rest of list")

;; Memq (uses eq? - identity comparison)
(assert-equal '(3 4 5) (memq 3 '(1 2 3 4 5)) "Memq finds element")
(assert-false (memq 6 '(1 2 3 4 5)) "Memq returns #f for missing element")
(assert-equal '(1 2 3) (memq 1 '(1 2 3)) "Memq finds first element")

;; Memv (uses eqv? - value comparison)
(assert-equal '(3 4 5) (memv 3 '(1 2 3 4 5)) "Memv finds element")
(assert-false (memv 6 '(1 2 3 4 5)) "Memv returns #f for missing element")

;; Member (uses equal? - structural comparison)
(assert-truthy (member '(4 5) '(1 2 3 (4 5) 6 7)) "Member finds nested list")
(assert-false (memq '(4 5) '(1 2 3 (4 5) 6 7)) "Memq cannot find nested list (different identity)")

;; Association lists - Assq (using cons to avoid dotted pair syntax)
(define alist (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))
(assert-equal (cons 'a 1) (assq 'a alist) "Assq finds pair")
(assert-equal (cons 'c 3) (assq 'c alist) "Assq finds last pair")
(assert-false (assq 'd alist) "Assq returns #f for missing key")

;; Assv
(define num-alist (list (cons 1 "one") (cons 2 "two") (cons 3 "three")))
(assert-equal (cons 2 "two") (assv 2 num-alist) "Assv finds numeric key")
(assert-false (assv 4 num-alist) "Assv returns #f for missing key")

;; Assoc
(define str-alist (list (cons "a" 1) (cons "b" 2) (cons "c" 3)))
(assert-equal (cons "b" 2) (assoc "b" str-alist) "Assoc finds string key")
(assert-false (assoc "d" str-alist) "Assoc returns #f for missing key")

;; Higher-order list functions
(test-suite "Higher-Order List Functions")

;; Map
(assert-equal '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)) "Map doubles elements")
(assert-equal '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)) "Map squares elements")
(assert-equal '() (map (lambda (x) x) '()) "Map on empty list")
(assert-equal '(#t #f #t) (map number? '(1 "a" 2)) "Map with predicate")

;; Filter
(assert-equal '(2 4 6) (filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6)) "Filter even numbers")
(assert-equal '() (filter (lambda (x) (> x 10)) '(1 2 3)) "Filter with no matches")
(assert-equal '(1 2 3) (filter (lambda (x) #t) '(1 2 3)) "Filter with always-true")

;; For-each
(define for-each-result '())
(for-each (lambda (x) (set! for-each-result (cons x for-each-result))) '(1 2 3))
(assert-equal '(3 2 1) for-each-result "For-each side effects")

;; Reduce
(assert-equal 15 (reduce + '(1 2 3 4 5)) "Reduce with addition")
(assert-equal 120 (reduce * '(1 2 3 4 5)) "Reduce with multiplication")
(assert-equal 5/8 (reduce / '(500 400 2)) "Reduce with division")

;; Count
(assert-equal 3 (count (lambda (x) (> x 2)) '(1 2 3 4 5)) "Count elements > 2")
(assert-equal 0 (count (lambda (x) (> x 10)) '(1 2 3 4 5)) "Count with no matches")
(assert-equal 5 (count number? '(1 2 3 4 5)) "Count all numbers")

(test-summary)
