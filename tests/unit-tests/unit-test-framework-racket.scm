#lang racket/base
;; VeloxVM Unit Test Framework - Racket Adapter
;; Platform-specific adapter for Racket (racket/base)

(require racket/promise  ; For delay/force support
         racket/include) ; For include directive

(provide (all-defined-out)
         (all-from-out racket/promise))  ; Re-export delay/force

;; Platform detection
(define running-on-veloxvm? #f)

;; Platform-specific compatibility: print procedure
;; Racket doesn't have VeloxVM's print, so we define it
(define (print . args)
  (for-each (lambda (arg)
              (cond
                ((char? arg) (display arg))
                ((eq? arg #\newline) (newline))
                (else (display arg))))
            args))

;; Platform-specific: Assert that an expression raises an error
;; Racket has with-handlers for exception handling
(define-syntax assert-error
  (syntax-rules ()
    ((assert-error expr description)
     (let ((error-raised #f))
       (with-handlers ((exn:fail? (lambda (e) (set! error-raised #t))))
         expr)
       (if error-raised
           (record-pass description)
           (record-fail description
                        (list "Expected: error to be raised"
                              "Actual: no error raised")))))))

;; Load shared core framework
;; Note: In Racket, we use require with relative path
(include "unit-test-framework-core.scm")
