;; VeloxVM Unit Tests - Letrec (R5RS mutually recursive bindings)
;; Tests for: letrec, mutually recursive functions

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Letrec - Basic Recursion")

;; Simple recursion - factorial
(assert-equal 120
  (letrec ((fact (lambda (n)
                   (if (<= n 1) 1 (* n (fact (- n 1)))))))
    (fact 5))
  "Factorial of 5")

;; Fibonacci
(assert-equal 55
  (letrec ((fib (lambda (n)
                  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
    (fib 10))
  "Fibonacci of 10")

;; List reverse with accumulator
(assert-equal '(5 4 3 2 1)
  (letrec ((rev (lambda (lst acc)
                  (if (null? lst) acc (rev (cdr lst) (cons (car lst) acc))))))
    (rev '(1 2 3 4 5) '()))
  "List reverse with accumulator")

;; GCD using Euclidean algorithm
(assert-equal 6
  (letrec ((gcd-impl (lambda (a b)
                       (if (= b 0) a (gcd-impl b (remainder a b))))))
    (gcd-impl 48 18))
  "GCD of 48 and 18")

(test-suite "Letrec - Mutual Recursion")

;; Mutually recursive even/odd - test even
(assert-true
  (letrec ((is-even (lambda (n)
                      (if (= n 0) #t (is-odd (- n 1)))))
           (is-odd (lambda (n)
                     (if (= n 0) #f (is-even (- n 1))))))
    (is-even 10))
  "Mutually recursive is-even returns true for 10")

;; Mutually recursive even/odd - test odd
(assert-true
  (letrec ((is-even (lambda (n)
                      (if (= n 0) #t (is-odd (- n 1)))))
           (is-odd (lambda (n)
                     (if (= n 0) #f (is-even (- n 1))))))
    (is-odd 11))
  "Mutually recursive is-odd returns true for 11")

;; Mutually recursive even/odd - test even false
(assert-false
  (letrec ((is-even (lambda (n)
                      (if (= n 0) #t (is-odd (- n 1)))))
           (is-odd (lambda (n)
                     (if (= n 0) #f (is-even (- n 1))))))
    (is-even 11))
  "Mutually recursive is-even returns false for 11")

;; Three mutually recursive functions
(assert-equal 9
  (letrec ((f (lambda (n) (if (= n 0) 0 (+ 1 (g (- n 1))))))
           (g (lambda (n) (if (= n 0) 0 (+ 1 (h (- n 1))))))
           (h (lambda (n) (if (= n 0) 0 (+ 1 (f (- n 1)))))))
    (f 9))
  "Three-way mutual recursion")

(test-suite "Letrec - Edge Cases")

;; Empty bindings
(assert-equal 42
  (letrec () 42)
  "Letrec with empty bindings returns body")

;; Letrec with single binding
(assert-equal 24
  (letrec ((double (lambda (x) (* x 2))))
    (double 12))
  "Letrec with single non-recursive binding")

;; Letrec shadowing outer variable
(define x 10)
(assert-equal 10
  (letrec ((x 5)
           (double (lambda () (* x 2))))
    (double))
  "Letrec bindings shadow outer variables")

;; Letrec with multiple body expressions
(assert-equal 15
  (letrec ((add (lambda (a b) (+ a b))))
    (define temp (add 3 5))
    (add temp 7))
  "Letrec with multiple body expressions")

(test-summary)
