;;; VeloxVM Unit Tests - R7RS cond-expand
;;; cond-expand is resolved at compile time against the static feature
;;; set in languages/scheme/library.rkt (kept in sync with the runtime
;;; (features) procedure in runtime/r7rs-features.scm) plus the set of
;;; known libraries.

(include "../unit-test-framework.scm")

(cond-expand
  (veloxvm (define ce-feature "velox"))
  (else    (define ce-feature "other")))

(cond-expand
  ((library (scheme base)) (define ce-lib "has-base"))
  (else                    (define ce-lib "no-base")))

(cond-expand
  (nonexistent-feature (define ce-fallback "wrong"))
  (else                (define ce-fallback "fallback")))

(cond-expand
  ((and veloxvm r5rs) (define ce-and "both"))
  (else               (define ce-and "not-both")))

(cond-expand
  ((or nonexistent veloxvm) (define ce-or "or-ok"))
  (else                     (define ce-or "or-fail")))

(cond-expand
  ((not nonexistent) (define ce-not "not-ok"))
  (else              (define ce-not "not-fail")))

(test-suite "R7RS cond-expand")

(assert-equal "velox"    ce-feature  "feature id selects matching clause")
(assert-equal "has-base" ce-lib      "(library ...) true for a standard library")
(assert-equal "fallback" ce-fallback "else taken when no requirement matches")
(assert-equal "both"     ce-and      "(and ...) requirement")
(assert-equal "or-ok"    ce-or       "(or ...) requirement")
(assert-equal "not-ok"   ce-not      "(not ...) requirement")

(test-summary)
