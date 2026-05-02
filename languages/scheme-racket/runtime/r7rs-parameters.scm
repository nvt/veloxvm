;;; ============================================================================
;;; VeloxVM R7RS Parameter Objects Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; A parameter is a procedure that returns its current value when
;;; called with no arguments and updates the value when called with
;;; one argument. The parameterize syntactic form (rewriter) uses this
;;; read/write convention to install and restore values across a
;;; dynamic extent.
;;;
;;; The optional converter argument from the full R7RS spec is not
;;; supported.
;;;
;;; The `include` directive currently has issues in nested scopes;
;;; copy this definition directly into your program.
;;; ============================================================================

(define (make-parameter init)
  (let ((value init))
    (lambda args
      (if (null? args)
          value
          (set! value (car args))))))

;;; End of r7rs-parameters.scm
