;; VeloxVM Unit Tests - Vector Extensions (VM-Specific)
;; Tests for VeloxVM-specific vector procedures: vector->string, vector-merge
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

(test-summary)
