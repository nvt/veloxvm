;; VeloxVM Unit Tests - String Extensions (VM-Specific)
;; Tests for VeloxVM-specific string procedures: string-compare, string-split
;; Note: R5RS provides string=?, string<?, etc. These are VeloxVM primitives.

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "String Extensions (VM-Specific)")

;; String-compare (VM-specific: returns -1/0/1)
;; Note: R5RS uses string=?, string<?, string>?, string<=?, string>=? predicates
(assert-equal 0 (string-compare "abc" "abc") "Equal strings compare to 0")
(assert-true (< (string-compare "abc" "abd") 0) "abc < abd")
(assert-true (> (string-compare "abd" "abc") 0) "abd > abc")
(assert-true (< (string-compare "ab" "abc") 0) "Shorter string is less")

;; String-split (VM-specific: not in R5RS)
(assert-equal '("Test") (string-split "Test" " ") "Split with no delimiter matches")
(assert-equal '("COMMAND" "ARG1" "ARG2") (string-split "COMMAND ARG1 ARG2" " ") "Split on space")
(assert-equal '("a" "b" "c") (string-split "a,b,c" ",") "Split on comma")

(test-summary)
