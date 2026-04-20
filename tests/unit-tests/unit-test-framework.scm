;; VeloxVM Unit Test Framework - Native Adapter
;; Platform-specific adapter for VeloxVM native implementation

;; Platform detection
(define running-on-veloxvm? #t)

;; Platform-specific: Assert that an expression raises an error
;; VeloxVM has guard/raise support (R6RS/R7RS)
(define-syntax assert-error
  (syntax-rules ()
    ((assert-error expr description)
     (let ((error-raised #f))
       (guard (exc (else (set! error-raised #t)))
         expr)
       (if error-raised
           (record-pass description)
           (record-fail description
                        (list "Expected: error to be raised"
                              "Actual: no error raised")))))))

;; Load shared core framework
(include "unit-test-framework-core.scm")
