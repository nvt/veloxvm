;; VeloxVM Unit Tests - Comparison Operations
;; Tests for: =, <, <=, >, >=, zero?

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Comparison Operations")

;; Equality tests (=)
(assert-true (= 5 5) "Equal integers are equal")
(assert-false (= 5 6) "Different integers are not equal")
(assert-true (= 0 0) "Zero equals zero")
(assert-true (= -5 -5) "Negative integers can be equal")
(assert-false (= 5 -5) "Positive and negative are not equal")
(assert-true (= 1 1 1 1) "Multiple equal values")
(assert-false (= 1 1 2 1) "Multiple values with one different")
(assert-true (= 1/2 2/4) "Equal rationals (normalized)")
(assert-true (= 3/3 1) "Rational equal to integer")

;; Inequality tests (not equal)
(assert-true (not (= 5 6)) "Different integers are not equal")
(assert-false (not (= 5 5)) "Equal integers are not different")
(assert-true (not (= 1 2 3 4)) "Multiple different values")
(assert-false (not (= 1 1)) "Same values are not different")

;; Less than tests (<)
(assert-true (< 3 5) "3 is less than 5")
(assert-false (< 5 3) "5 is not less than 3")
(assert-false (< 5 5) "5 is not less than 5")
(assert-true (< -10 -5) "-10 is less than -5")
(assert-true (< -5 5) "Negative is less than positive")
(assert-true (< 1 2 3 4 5) "Multiple values in ascending order")
(assert-false (< 1 2 3 3 5) "Not strictly ascending (duplicate)")
(assert-true (< 1/4 1/2) "Rational comparison")
(assert-true (< 1/3 1/2) "Rational comparison with different denominators")

;; Less than or equal tests (<=)
(assert-true (<= 3 5) "3 is less than or equal to 5")
(assert-true (<= 5 5) "5 is less than or equal to 5")
(assert-false (<= 6 5) "6 is not less than or equal to 5")
(assert-true (<= -10 -10) "Equal negatives")
(assert-true (<= 1 2 2 3 4) "Multiple values with duplicates")
(assert-false (<= 1 2 3 2 4) "Out of order")

;; Greater than tests (>)
(assert-true (> 5 3) "5 is greater than 3")
(assert-false (> 3 5) "3 is not greater than 5")
(assert-false (> 5 5) "5 is not greater than 5")
(assert-true (> -5 -10) "-5 is greater than -10")
(assert-true (> 5 -5) "Positive is greater than negative")
(assert-true (> 5 4 3 2 1) "Multiple values in descending order")
(assert-false (> 5 4 4 2 1) "Not strictly descending (duplicate)")
(assert-true (> 1/2 1/4) "Rational comparison")

;; Greater than or equal tests (>=)
(assert-true (>= 5 3) "5 is greater than or equal to 3")
(assert-true (>= 5 5) "5 is greater than or equal to 5")
(assert-false (>= 3 5) "3 is not greater than or equal to 5")
(assert-true (>= -5 -10) "-5 is >= -10")
(assert-true (>= 5 4 4 3 2) "Multiple values with duplicates")

;; Zero? tests
(assert-true (zero? 0) "0 is zero")
(assert-false (zero? 1) "1 is not zero")
(assert-false (zero? -1) "-1 is not zero")
(assert-true (zero? 0/1) "0/1 is zero")
(assert-false (zero? 1/1000000) "Small rational is not zero")

;; Mixed type comparisons
(assert-true (= 2 4/2) "Integer equals equivalent rational")
(assert-true (< 1/3 1) "Rational less than integer")
(assert-true (> 2 3/4) "Integer greater than rational")

(test-summary)
