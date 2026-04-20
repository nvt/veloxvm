;; VeloxVM Unit Tests - Type Predicates and Equality
;; Tests for: number?, integer?, rational?, real?, complex?, exact?, inexact?,
;;            procedure?, boolean?, symbol?, port?, not, eq?, eqv?, equal?

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Number Type Predicates")

;; Number?
(assert-true (number? 42) "Integer is a number")
(assert-true (number? -17) "Negative integer is a number")
(assert-true (number? 3/4) "Rational is a number")
(assert-true (number? 0) "Zero is a number")
(assert-false (number? "42") "String is not a number")
(assert-false (number? 'forty-two) "Symbol is not a number")
(assert-false (number? '(1 2 3)) "List is not a number")

;; Integer?
(assert-true (integer? 42) "42 is an integer")
(assert-true (integer? -17) "-17 is an integer")
(assert-true (integer? 0) "0 is an integer")
(assert-false (integer? 3/4) "3/4 is not an integer")
(assert-true (integer? 4/2) "4/2 normalizes to integer")
(assert-false (integer? "42") "String is not an integer")

;; Rational?
(assert-true (rational? 3/4) "3/4 is rational")
(assert-true (rational? 42) "Integer is also rational")
(assert-true (rational? -17/3) "Negative rational")
(assert-false (rational? "3/4") "String is not rational")

;; Exact?
(assert-true (exact? 42) "Integer is exact")
(assert-true (exact? 3/4) "Rational is exact")
(assert-true (exact? -17) "Negative integer is exact")

(test-suite "Other Type Predicates")

;; Procedure?
(assert-true (procedure? (lambda (x) x)) "Lambda is a procedure")
(assert-true (procedure? +) "Built-in + is a procedure")
(assert-true (procedure? car) "Built-in car is a procedure")
(assert-false (procedure? 42) "Number is not a procedure")
(assert-false (procedure? '(1 2 3)) "List is not a procedure")
(assert-false (procedure? "hello") "String is not a procedure")

;; Boolean?
(assert-true (boolean? #t) "True is a boolean")
(assert-true (boolean? #f) "False is a boolean")
(assert-false (boolean? 0) "Zero is not a boolean")
(assert-false (boolean? 1) "One is not a boolean")
(assert-false (boolean? '()) "Empty list is not a boolean")
(assert-false (boolean? "true") "String is not a boolean")

;; Symbol? (R5RS §6.3.3)
(assert-true (symbol? 'foo) "Symbol 'foo is a symbol")
(assert-true (symbol? 'bar) "Symbol 'bar is a symbol")
(assert-true (symbol? (car '(a b c))) "Symbol from quote is a symbol")
(assert-false (symbol? "foo") "String is not a symbol")
(assert-false (symbol? 42) "Number is not a symbol")
(assert-false (symbol? '(a b c)) "List is not a symbol")

;; Not
(assert-true (not #f) "Not false is true")
(assert-false (not #t) "Not true is false")
(assert-false (not 1) "Not truthy value is false")
(assert-false (not "hello") "Not string is false")
(assert-false (not '(1 2 3)) "Not list is false")
(assert-true (not (not #t)) "Double negation")

(test-suite "Equality Predicates")

;; Eq? (identity comparison)
(assert-true (eq? 'a 'a) "Same symbol is eq?")
(assert-true (eq? #t #t) "True is eq? to true")
(assert-true (eq? #f #f) "False is eq? to false")
(assert-true (eq? '() '()) "Empty lists are eq?")
(assert-false (eq? (list 1 2) (list 1 2)) "Different list objects not eq?")
;; Note: VeloxVM interns string literals, so identical strings ARE eq?
;; This is R5RS-compliant implementation-defined behavior (R5RS 6.2)
(assert-true (eq? "hello" "hello") "VeloxVM interns string literals")

;; Eqv? (equivalence)
(assert-true (eqv? 1 1) "Same integers are eqv?")
(assert-true (eqv? #\a #\a) "Same characters are eqv?")
(assert-true (eqv? 'symbol 'symbol) "Same symbols are eqv?")
(assert-true (eqv? "Alpha" "Alpha") "Same strings are eqv?")
(assert-false (eqv? "Alpha" "Beta") "Different strings not eqv?")
(assert-false (eqv? (list 1 2 3) (list 1 2 3)) "Different list objects not eqv?")
(assert-false (eqv? (vector 1 2 3) (vector 1 2 3)) "Different vector objects not eqv?")

;; Equal? (structural equality)
(assert-true (equal? '(1 2 3) '(1 2 3)) "Equal lists are equal?")
(assert-true (equal? '(1 (2 3) 4) '(1 (2 3) 4)) "Nested equal lists")
(assert-true (equal? '#(1 2 3) '#(1 2 3)) "Equal vectors are equal?")
(assert-true (equal? '#(1 2 #(3 4)) '#(1 2 #(3 4))) "Nested equal vectors")
(assert-true (equal? "hello" "hello") "Equal strings are equal?")
(assert-false (equal? '(1 2 3) '(1 2 4)) "Different lists not equal?")
(assert-false (equal? '(1 (2 3 "Hej" 5) 4) '(1 (2 3 "Hej" (/ 10 2)) 4)) "Deep difference detected")

(test-suite "Mixed Type Comparisons")

;; Numbers
(assert-true (eqv? 2 4/2) "Integer equals equivalent rational with eqv?")
(assert-true (equal? 2 4/2) "Integer equals equivalent rational with equal?")

;; Lists vs vectors
(assert-false (equal? '(1 2 3) '#(1 2 3)) "List and vector not equal even with same elements")

;; Symbols
(assert-true (eq? 'hello 'hello) "Same symbols eq?")
(assert-true (eqv? 'hello 'hello) "Same symbols eqv?")
(assert-true (equal? 'hello 'hello) "Same symbols equal?")
(assert-false (equal? 'hello 'world) "Different symbols not equal?")

(test-suite "Type Checking Practical Examples")

;; Type dispatch function
(define (type-of x)
  (cond
    ((boolean? x) 'boolean)
    ((integer? x) 'integer)
    ((rational? x) 'rational)
    ((number? x) 'number)
    ((char? x) 'character)
    ((string? x) 'string)
    ((vector? x) 'vector)
    ((null? x) 'null)      ; Check null? BEFORE list? since '() is both
    ((list? x) 'list)
    ((pair? x) 'pair)
    ((procedure? x) 'procedure)
    (else 'unknown)))

(assert-equal 'boolean (type-of #t) "Type of true")
(assert-equal 'boolean (type-of #f) "Type of false")
(assert-equal 'integer (type-of 42) "Type of integer")
(assert-equal 'rational (type-of 3/4) "Type of rational")
(assert-equal 'character (type-of #\a) "Type of character")
(assert-equal 'string (type-of "hello") "Type of string")
(assert-equal 'vector (type-of '#(1 2 3)) "Type of vector")
(assert-equal 'list (type-of '(1 2 3)) "Type of list")
(assert-equal 'null (type-of '()) "Type of empty list")
(assert-equal 'procedure (type-of +) "Type of procedure")

;; Safe arithmetic
(define (safe-add a b)
  (if (and (number? a) (number? b))
      (+ a b)
      'error))

(assert-equal 5 (safe-add 2 3) "Safe add with numbers")
(assert-equal 'error (safe-add 2 "3") "Safe add with string returns error")
(assert-equal 'error (safe-add "2" 3) "Safe add with string returns error")

(test-summary)
