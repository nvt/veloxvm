;;; VeloxVM Unit Tests - R7RS features procedure
;;; Tests for: features (returns a list of supported feature symbols).

(include "../unit-test-framework.scm")

;; Inline definition from languages/scheme-racket/runtime/r7rs-features.scm
(define (features)
  '(veloxvm
    r5rs
    r7rs-subset
    exact-closed
    ratios))

(test-suite "R7RS features procedure")

(assert-equal #t (list? (features))
              "features returns a list")

;; memq returns the tail starting at the match, or #f if missing.
;; Wrap with pair? to get a clean boolean for assertions.
(assert-equal #t (pair? (memq 'veloxvm (features)))
              "features includes 'veloxvm")

(assert-equal #t (pair? (memq 'r5rs (features)))
              "features includes 'r5rs")

(assert-equal #t (pair? (memq 'r7rs-subset (features)))
              "features includes 'r7rs-subset")

(assert-equal #f (memq 'tabling (features))
              "features does not falsely advertise 'tabling")

(assert-equal #f (memq 'eval (features))
              "features does not falsely advertise 'eval")

(test-summary)
