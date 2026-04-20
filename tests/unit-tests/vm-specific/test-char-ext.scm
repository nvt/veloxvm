;; VeloxVM Unit Tests - Character Extensions (VM-Specific)
;; Tests for VeloxVM-specific character procedures: char-compare
;; Note: R5RS provides char=?, char<?, etc. These are VeloxVM primitives.

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Character Extensions (VM-Specific)")

;; Char-compare (VM-specific: returns -1/0/1)
;; Note: R5RS uses char=?, char<?, char>?, char<=?, char>=? predicates
(assert-equal 0 (char-compare #\a #\a) "Same characters compare to 0")
(assert-true (< (char-compare #\a #\b) 0) "a < b")
(assert-true (> (char-compare #\b #\a) 0) "b > a")
(assert-true (< (char-compare #\A #\a) 0) "A < a (uppercase before lowercase)")

(test-summary)
