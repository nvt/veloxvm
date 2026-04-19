;; VeloxVM Unit Tests - Unimplemented R5RS Features
;; Tests for R5RS procedures not yet implemented in VeloxVM
;; These tests are SKIPPED on VeloxVM but run on MIT Scheme and Racket

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Unimplemented R5RS Features")

;; =============================================================================
;; Promises (R5RS §6.4) - delay and force
;; =============================================================================

(test-suite "Promises (delay/force) - R5RS §6.4")

;; Basic delay/force
(if running-on-veloxvm?
    (assert-skip "Force evaluates delayed expression"
                 "delay/force not implemented in VeloxVM")
    (let ((p (delay (+ 1 2))))
      (assert-equal 3 (force p) "Force evaluates delayed expression")))

;; Delay doesn't evaluate immediately
(if running-on-veloxvm?
    (assert-skip "Delay doesn't evaluate immediately"
                 "delay/force not implemented in VeloxVM")
    (let ((count 0))
      (define p (delay (begin (set! count (+ count 1)) count)))
      (assert-equal 0 count "Delay doesn't evaluate immediately")
      (force p)
      (assert-equal 1 count "Force evaluates the delayed expression")))

;; Force caches result (memoization)
(if running-on-veloxvm?
    (assert-skip "Force only evaluates once (memoization)"
                 "delay/force not implemented in VeloxVM")
    (let ((count 0))
      (define p (delay (begin (set! count (+ count 1)) count)))
      (force p)
      (force p)
      (assert-equal 1 count "Force only evaluates once (memoization)")))

;; Nested delays
(if running-on-veloxvm?
    (assert-skip "Nested delays work"
                 "delay/force not implemented in VeloxVM")
    (let ((p (delay (delay (+ 1 2)))))
      (assert-equal 3 (force (force p)) "Nested delays work")))

;; =============================================================================
;; File I/O with call-with-*-file (R5RS §6.6.1)
;; =============================================================================

(test-suite "File I/O - call-with-*-file - R5RS §6.6.1")

;; Note: VeloxVM has open-input-file/open-output-file but not call-with-*-file
;; These can be implemented in Scheme as library procedures

(assert-skip "call-with-input-file not implemented"
             "Not a VM primitive (can be implemented as library procedure)")

(assert-skip "call-with-output-file not implemented"
             "Not a VM primitive (can be implemented as library procedure)")

;; =============================================================================
;; Environment Specifiers (R5RS §6.5)
;; =============================================================================

(test-suite "Environment Specifiers - R5RS §6.5")

(assert-skip "scheme-report-environment not implemented"
             "Not implemented in VeloxVM")

(assert-skip "null-environment not implemented"
             "Not implemented in VeloxVM")

(assert-skip "interaction-environment not implemented"
             "Not implemented in VeloxVM")

;; =============================================================================
;; Load (R5RS §6.6.4)
;; =============================================================================

(test-suite "Load - R5RS §6.6.4")

;; Note: VeloxVM has load-program which is different from R5RS load
(assert-skip "load not implemented"
             "VeloxVM has load-program instead")

;; =============================================================================
;; Summary
;; =============================================================================

(test-summary)

;; Summary of unimplemented R5RS features:
;; - delay/force (promises) - R5RS §6.4
;; - call-with-input-file, call-with-output-file - R5RS §6.6.1 (can be library)
;; - scheme-report-environment, null-environment, interaction-environment - R5RS §6.5
;; - load - R5RS §6.6.4 (VeloxVM has load-program)
;;
;; Note: Most of these can be implemented as library procedures in Scheme
;; without requiring new VM primitives.
