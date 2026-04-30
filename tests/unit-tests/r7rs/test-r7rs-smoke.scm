;;; VeloxVM Unit Tests - R7RS smoke test
;;; Confirms the r7rs/ test directory is wired into the runner.
;;; Real R7RS coverage lives in the per-feature test files alongside this one.

(include "../unit-test-framework.scm")

(test-suite "R7RS smoke")

(assert-equal 1 1 "trivial integer equality")
(assert-equal '() '() "empty-list equality")

(test-summary)
