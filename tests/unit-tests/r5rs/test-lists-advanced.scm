;; VeloxVM Unit Tests - R5RS Advanced List Operations
;; Tests for: Compound accessors, list-tail, list-ref, member, assoc

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Compound Car/Cdr Accessors")

;; R5RS §6.3.2 - Compound accessors
(assert-equal 1 (caar '((1 2) 3)) "caar extracts car of car")
(assert-equal 2 (cadr '(1 2 3)) "cadr extracts car of cdr")
(assert-equal '(2) (cdar '((1 2) 3)) "cdar extracts cdr of car")
(assert-equal '(3) (cddr '(1 2 3)) "cddr extracts cdr of cdr")

;; Three-level accessors
(assert-equal 1 (caaar '(((1 2) 3) 4)) "caaar triple car")
(assert-equal 2 (caadr '(1 (2 3) 4)) "caadr car-car-cdr")
(assert-equal 2 (cadar '((1 2 3) 4)) "cadar car-cdr-car")
(assert-equal '(3) (cddar '((1 2 3) 4)) "cddar cdr-cdr-car")

;; Four-level accessors
(assert-equal 1 (caaaar '((((1 2) 3) 4) 5)) "caaaar quad car")
(assert-equal 2 (caaadr '(1 ((2 3) 4) 5)) "caaadr car-car-car-cdr")
(assert-equal 2 (caadar '((1 (2 3) 4) 5)) "caadar car-car-cdr-car")
(assert-equal 3 (caaddr '(1 2 (3 4 5) 6)) "caaddr car-car-cdr-cdr")

(test-suite "List-Tail")

;; R5RS §6.3.2 - list-tail returns tail of list after k elements
(assert-equal '(1 2 3) (list-tail '(1 2 3) 0) "list-tail 0 returns whole list")
(assert-equal '(2 3) (list-tail '(1 2 3) 1) "list-tail 1 drops first element")
(assert-equal '(3) (list-tail '(1 2 3) 2) "list-tail 2 drops two elements")
(assert-equal '() (list-tail '(1 2 3) 3) "list-tail at length returns empty")

;; Edge cases
(assert-equal '(a b c d e) (list-tail '(a b c d e) 0) "list-tail 0 on longer list")
(assert-equal '(d e) (list-tail '(a b c d e) 3) "list-tail drops multiple")

(test-suite "List-Ref")

;; R5RS §6.3.2 - list-ref returns element at index k
(assert-equal 'a (list-ref '(a b c d) 0) "list-ref first element")
(assert-equal 'b (list-ref '(a b c d) 1) "list-ref second element")
(assert-equal 'c (list-ref '(a b c d) 2) "list-ref third element")
(assert-equal 'd (list-ref '(a b c d) 3) "list-ref last element")

;; Numeric lists
(assert-equal 10 (list-ref '(10 20 30 40) 0) "list-ref numbers first")
(assert-equal 30 (list-ref '(10 20 30 40) 2) "list-ref numbers middle")

(test-suite "Member")

;; R5RS §6.3.2 - member finds element using equal?
(assert-equal '(b c) (member 'b '(a b c)) "member finds element")
(assert-equal #f (member 'd '(a b c)) "member returns #f if not found")
(assert-equal '(a b c) (member 'a '(a b c)) "member at start")
(assert-equal '(c) (member 'c '(a b c)) "member at end")

;; member with different types
(assert-equal '(2 3 4) (member 2 '(1 2 3 4)) "member finds number")
(assert-equal '("foo" "bar") (member "foo" '("baz" "foo" "bar")) "member finds string")

;; member returns rest of list from found element
(define found (member 'x '(a b x c d)))
(assert-equal '(x c d) found "member returns tail from match")
(assert-equal 'x (car found) "member result car is matched element")

(test-suite "Memq and Memv")

;; memq uses eq? (symbol/object identity)
(assert-equal '(b c) (memq 'b '(a b c)) "memq finds symbol")
(assert-equal #f (memq 'd '(a b c)) "memq returns #f if not found")

;; memv uses eqv? (primitive equality)
(assert-equal '(2 3) (memv 2 '(1 2 3)) "memv finds number")
(assert-equal #f (memv 4 '(1 2 3)) "memv returns #f if not found")

;; Difference between member, memq, memv
(assert-equal '(b c) (memq 'b '(a b c)) "memq with symbols")
(assert-equal '(2 3) (memv 2 '(1 2 3)) "memv with numbers")

(test-suite "Assoc")

;; R5RS §6.3.2 - assoc finds key-value pair using equal?
(assert-equal '(b 2) (assoc 'b '((a 1) (b 2) (c 3))) "assoc finds pair")
(assert-equal #f (assoc 'd '((a 1) (b 2) (c 3))) "assoc returns #f if not found")
(assert-equal '(a 1) (assoc 'a '((a 1) (b 2) (c 3))) "assoc first element")

;; assoc with complex values
(assert-equal '(x 10 20) (assoc 'x '((y 5 6) (x 10 20) (z 30 40))) "assoc with multi-element values")

;; assoc returns whole pair
(define pair (assoc 'name '((age 25) (name "Alice") (city "NYC"))))
(assert-equal '(name "Alice") pair "assoc returns complete pair")
(assert-equal 'name (car pair) "assoc result car is key")
(assert-equal "Alice" (cadr pair) "assoc result cadr is value")

(test-suite "Assq and Assv")

;; assq uses eq? (symbol/object identity)
(assert-equal '(b 2) (assq 'b '((a 1) (b 2) (c 3))) "assq finds pair")
(assert-equal #f (assq 'd '((a 1) (b 2) (c 3))) "assq returns #f if not found")

;; assv uses eqv? (primitive equality)
(assert-equal '(2 two) (assv 2 '((1 one) (2 two) (3 three))) "assv finds numeric key")
(assert-equal #f (assv 4 '((1 one) (2 two) (3 three))) "assv returns #f if not found")

;; Association list as dictionary
(define dict '((name "Bob") (age 30) (city "SF")))
(assert-equal '(name "Bob") (assq 'name dict) "assq lookup name")
(assert-equal '(age 30) (assq 'age dict) "assq lookup age")
(assert-equal '(city "SF") (assq 'city dict) "assq lookup city")

(test-suite "List Searching Patterns")

;; Combining list operations
(define data '((id 1) (name "Alice") (id 2) (name "Bob")))
(assert-equal '(id 1) (assq 'id data) "assq finds first matching key")

;; Using member to check membership
(define colors '(red green blue))
(assert-equal '(red green blue) (member 'red colors) "member finds color in list")
(assert-equal #f (member 'yellow colors) "member returns #f for missing color")

;; Nested lists
(define nested '((a (1 2)) (b (3 4)) (c (5 6))))
(assert-equal '(b (3 4)) (assq 'b nested) "assq on nested structure")
(assert-equal '(3 4) (cadr (assq 'b nested)) "extract nested value")

(test-suite "Edge Cases")

;; Empty list handling
(assert-equal #f (member 'x '()) "member on empty list")
(assert-equal #f (memq 'x '()) "memq on empty list")
(assert-equal #f (assoc 'x '()) "assoc on empty list")

;; Single element
(assert-equal '(x) (member 'x '(x)) "member single element match")
(assert-equal #f (member 'y '(x)) "member single element no match")

(test-summary)
