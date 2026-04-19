;; VeloxVM Unit Tests - Arithmetic Operations
;; Tests for: +, -, *, /, gcd, lcm, numerator, denominator, quotient, remainder, modulo

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Arithmetic Operations")

;; Addition tests
(assert-equal 0 (+) "Addition with no arguments returns 0")
(assert-equal 5 (+ 5) "Addition with single argument returns that argument")
(assert-equal 10 (+ 3 7) "Addition of two positive integers")
(assert-equal -4 (+ -10 6) "Addition of negative and positive integer")
(assert-equal 0 (+ -5 5) "Addition resulting in zero")
(assert-equal 100000000 (+ 100000000 0) "Addition with large numbers")
(assert-equal 99999999 (+ 100000000 -1) "Addition with large number and -1")
(assert-equal 15 (+ 1 2 3 4 5) "Addition with multiple arguments")
(assert-equal 1/2 (+ 1/4 1/4) "Addition of rationals")
(assert-equal 8/15 (+ 2/10 4/12) "Addition of rationals with different denominators")

;; Subtraction tests
(assert-equal -5 (- 5) "Negation of single argument")
(assert-equal 3 (- 10 7) "Subtraction of two positive integers")
(assert-equal -16 (- -10 6) "Subtraction with negative first argument")
(assert-equal 1 (- -1 -2) "Subtraction of two negative numbers")
(assert-equal 0 (- 5 5) "Subtraction resulting in zero")
(assert-equal 5 (- 10 3 2) "Subtraction with multiple arguments")

;; Multiplication tests
(assert-equal 1 (*) "Multiplication with no arguments returns 1")
(assert-equal 7 (* 7) "Multiplication with single argument")
(assert-equal 21 (* 3 7) "Multiplication of two positive integers")
(assert-equal -60 (* -10 6) "Multiplication of negative and positive")
(assert-equal 100 (* -10 -10) "Multiplication of two negatives")
(assert-equal 0 (* 1000 0) "Multiplication by zero")
(assert-equal 120 (* 1 2 3 4 5) "Multiplication with multiple arguments")
(assert-equal 1/6 (* 1/2 1/3) "Multiplication of rationals")

;; Division tests
(assert-equal 1/5 (/ 5) "Division with single argument returns reciprocal")
(assert-equal 5 (/ 10 2) "Simple division")
(assert-equal 208/5 (/ 208 5) "Division resulting in rational")
(assert-equal -5 (/ 10 -2) "Division with negative divisor")
(assert-equal 5 (/ -10 -2) "Division of two negatives")
(assert-equal 2 (/ 100 10 5) "Division with multiple arguments")
(assert-equal -5 (* (/ 10 2) (- -3 -2)) "Complex expression with division and subtraction")

;; GCD tests
(assert-equal 0 (gcd) "GCD with no arguments returns 0")
(assert-equal 5 (gcd 5) "GCD of single number")
(assert-equal 6 (gcd 54 24) "GCD of 54 and 24")
(assert-equal 1 (gcd 17 13) "GCD of coprime numbers")
(assert-equal 10 (gcd 100 10) "GCD where one divides the other")
(assert-equal 5 (gcd 15 25 35) "GCD of multiple numbers")

;; LCM tests
(assert-equal 1 (lcm) "LCM with no arguments returns 1")
(assert-equal 5 (lcm 5) "LCM of single number")
(assert-equal 60 (lcm 5 4 3 2 1) "LCM of 1 through 5")
(assert-equal 12 (lcm 4 6) "LCM of 4 and 6")
(assert-equal 100 (lcm 100 10) "LCM where one divides the other")

;; Numerator and Denominator tests
(assert-equal 3 (numerator 3/4) "Numerator of 3/4")
(assert-equal 4 (denominator 3/4) "Denominator of 3/4")
(assert-equal 5 (numerator 5) "Numerator of integer")
(assert-equal 1 (denominator 5) "Denominator of integer is 1")
(assert-equal 1 (numerator 1/3) "Numerator of 1/3")
(assert-equal 3 (denominator 1/3) "Denominator of 1/3")

;; Quotient tests (using variables to avoid constant folding issues)
(define n10 10)
(define n3 3)
(define n-3 -3)
(define n-10 -10)
(define n25 25)
(define n5 5)

(assert-equal 3 (quotient n10 n3) "Quotient of 10 / 3")
(assert-equal -3 (quotient n10 n-3) "Quotient with negative divisor")
(assert-equal -3 (quotient n-10 n3) "Quotient with negative dividend")
(assert-equal 3 (quotient n-10 n-3) "Quotient of two negatives")
(assert-equal 5 (quotient n25 n5) "Exact quotient")

;; Remainder tests
(assert-equal 1 (remainder n10 n3) "Remainder of 10 / 3")
(assert-equal 1 (remainder n10 n-3) "Remainder with negative divisor")
(assert-equal -1 (remainder n-10 n3) "Remainder with negative dividend")
(assert-equal -1 (remainder n-10 n-3) "Remainder of two negatives")
(assert-equal 0 (remainder n25 n5) "Remainder when exactly divisible")

;; Modulo tests
(assert-equal 1 (modulo n10 n3) "Modulo of 10 % 3")
(assert-equal -2 (modulo n10 n-3) "Modulo with negative divisor")
(assert-equal 2 (modulo n-10 n3) "Modulo with negative dividend")
(assert-equal -1 (modulo n-10 n-3) "Modulo of two negatives")
(assert-equal 0 (modulo n25 n5) "Modulo when exactly divisible")

;; Complex arithmetic expressions
(assert-equal 210 (+ (+ 1 (+ (+ 2 3 4 5) 6 7 8) 9 10 11 12 13) 14 15 16 (+ 17 (+ 18 19)) 20) "Complex nested addition")

(test-summary)
