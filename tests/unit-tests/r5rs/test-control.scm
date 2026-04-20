;; VeloxVM Unit Tests - Control Flow and Special Forms
;; Tests for: if, begin, define, set!, and, or, quote, apply, cond, case, let, let*, letrec

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Control Flow - Basic")

;; If
(assert-equal 'yes (if #t 'yes 'no) "If with true condition")
(assert-equal 'no (if #f 'yes 'no) "If with false condition")
(assert-equal 'yes (if 1 'yes 'no) "If with truthy value (1)")
(assert-equal 'yes (if "string" 'yes 'no) "If with truthy value (string)")
(assert-equal 'only (if #t 'only) "If without else clause (true)")

;; Begin
(assert-equal 3 (begin 1 2 3) "Begin returns last expression")
(assert-equal 'done
  (begin
    (define x 1)
    (set! x (+ x 1))
    'done)
  "Begin with side effects")

;; Define and Set!
(define test-var 10)
(assert-equal 10 test-var "Define creates variable")
(set! test-var 20)
(assert-equal 20 test-var "Set! modifies variable")

;; Define procedure
(define (square x) (* x x))
(assert-equal 25 (square 5) "Define procedure")
(define double (lambda (x) (* x 2)))
(assert-equal 10 (double 5) "Define lambda")

;; And
(assert-true (and) "And with no arguments returns #t")
(assert-true (and #t) "And with single true")
(assert-false (and #f) "And with single false")
(assert-true (and #t #t #t) "And with all true")
(assert-false (and #t #f #t) "And short-circuits on false")
(assert-equal 3 (and 1 2 3) "And returns last value if all truthy")

;; Or
(assert-false (or) "Or with no arguments returns #f")
(assert-true (or #t) "Or with single true")
(assert-false (or #f) "Or with single false")
(assert-true (or #f #t #f) "Or with one true")
(assert-false (or #f #f #f) "Or with all false")
(assert-equal 1 (or 1 2 3) "Or returns first truthy value")
(assert-equal 3 (or #f #f 3) "Or returns first truthy value (at end)")

;; Quote
(assert-equal 'hello (quote hello) "Quote symbol")
(assert-equal '(1 2 3) (quote (1 2 3)) "Quote list")
(assert-equal '(+ 1 2) (quote (+ 1 2)) "Quote prevents evaluation")
(assert-equal 'hello 'hello "Quote shorthand")

;; Apply
(assert-equal 6 (apply + '(1 2 3)) "Apply with list of arguments")
(assert-equal '(1 2 3) (apply list '(1 2 3)) "Apply list function")
(assert-equal 10 (apply * '(2 5)) "Apply multiplication")

(test-suite "Control Flow - Conditionals")

;; Cond
(assert-equal 'greater
  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less))
  "Cond with first clause true")

(assert-equal 'equal
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal))
  "Cond with else clause")

(assert-equal 'second
  (cond ((= 1 2) 'first)
        ((= 2 2) 'second)
        (else 'third))
  "Cond with second clause true")

;; Case
(assert-equal "Small"
  (case 2
    ((1 2 3) "Small")
    ((4 5 6) "Medium")
    (else "Large"))
  "Case with first clause match")

(assert-equal "Medium"
  (case 5
    ((1 2 3) "Small")
    ((4 5 6) "Medium")
    (else "Large"))
  "Case with second clause match")

(assert-equal "Large"
  (case 100
    ((1 2 3) "Small")
    ((4 5 6) "Medium")
    (else "Large"))
  "Case with else clause")

(test-suite "Control Flow - Let Forms")

;; Let
(assert-equal 3
  (let ((x 1) (y 2))
    (+ x y))
  "Simple let")

(assert-equal 10
  (let ((x 5))
    (* x 2))
  "Let with single binding")

;; Let with shadowing
(define outer 100)
(assert-equal 5
  (let ((outer 5))
    outer)
  "Let shadows outer variable")
(assert-equal 100 outer "Outer variable unchanged after let")

;; Let*
(assert-equal 6
  (let* ((x 2)
         (y (* x 3)))
    y)
  "Let* with sequential bindings")

(assert-equal 14
  (let* ((a 1)
         (b (+ a 2))
         (c (+ a b 2))
         (d (+ a b c 4)))
    d)
  "Let* with multiple sequential bindings")

;; Letrec (for recursive definitions)
(assert-true
  (letrec ((even?
            (lambda (n)
              (if (zero? n)
                  #t
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (zero? n)
                  #f
                  (even? (- n 1))))))
    (even? 10))
  "Letrec with mutually recursive functions (even)")

(assert-false
  (letrec ((even?
            (lambda (n)
              (if (zero? n)
                  #t
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (zero? n)
                  #f
                  (even? (- n 1))))))
    (even? 11))
  "Letrec with mutually recursive functions (odd)")

;; Named let (loop)
(assert-equal '((6 1 3) (-5 -2))
  (let loop ((numbers '(3 -2 1 6 -5))
             (nonneg '())
             (neg '()))
    (cond ((null? numbers) (list nonneg neg))
          ((>= (car numbers) 0)
           (loop (cdr numbers)
                 (cons (car numbers) nonneg)
                 neg))
          ((< (car numbers) 0)
           (loop (cdr numbers)
                 nonneg
                 (cons (car numbers) neg)))))
  "Named let partitions list")

(test-suite "Control Flow - Loops")

;; Do loop
(assert-equal '#(0 1 2 3 4)
  (do ((vec (make-vector 5))
       (i 0 (+ i 1)))
      ((= i 5) vec)
    (vector-set! vec i i))
  "Do loop fills vector")

;; When and Unless (if available)
(define when-result 0)
(when (> 5 3)
  (set! when-result 1))
(assert-equal 1 when-result "When executes when condition is true")

(define unless-result 0)
(unless (< 5 3)
  (set! unless-result 1))
(assert-equal 1 unless-result "Unless executes when condition is false")

(test-suite "Control Flow - Lambda")

;; Lambda basic
(assert-equal 0 ((lambda (x y) (+ x y)) -9 9) "Lambda immediate application")
(assert-equal 25 ((lambda (x) (* x x)) 5) "Lambda square")

;; Lambda with multiple expressions
(assert-equal 10
  ((lambda (x)
     (define y (* x 2))
     y)
   5)
  "Lambda with internal define")

;; Higher-order functions with lambda
(assert-equal '(2 4 6)
  (map (lambda (x) (* x 2)) '(1 2 3))
  "Lambda in map")

(assert-equal '(2 4 6)
  (filter (lambda (x) (= 0 (modulo x 2))) '(1 2 3 4 5 6))
  "Lambda in filter")

(test-summary)
