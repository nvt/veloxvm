;; VeloxVM Unit Test Framework - MIT Scheme Adapter
;; Platform-specific adapter for MIT Scheme (pure R5RS)

;; Platform detection
(define running-on-veloxvm? #f)

;; Platform-specific compatibility: print procedure
;; MIT Scheme doesn't have VeloxVM's print, so we define it
(define (print . args)
  (for-each display args))

;; Platform-specific: Assert that an expression raises an error
;; MIT Scheme doesn't have guard/raise in base, so we skip these tests
(define-syntax assert-error
  (syntax-rules ()
    ((assert-error expr description)
     (record-skip description "guard not available in R5RS"))))

;; Load shared core framework
(include "unit-test-framework-core.scm")
