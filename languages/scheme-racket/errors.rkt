#lang racket

;; VeloxVM Racket Compiler - Error Handling
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Provides better error messages with context

(provide compile-error
         syntax-error
         runtime-error
         show-error-context
         with-error-context)

;; ============================================================================
;; Error Context Management
;; ============================================================================

(define current-source-file (make-parameter #f))
(define current-expression (make-parameter #f))

(define-syntax-rule (with-error-context file expr body ...)
  (parameterize ([current-source-file file]
                 [current-expression expr])
    body ...))

;; ============================================================================
;; Error Reporting
;; ============================================================================

(define (format-expression expr [max-length 60])
  "Format expression for error messages, truncating if too long"
  (let* ([str (format "~a" expr)]
         [len (string-length str)])
    (if (> len max-length)
        (string-append (substring str 0 max-length) "...")
        str)))

(define (show-error-context)
  "Show the current error context"
  (when (current-source-file)
    (fprintf (current-error-port) "  in file: ~a\n" (current-source-file)))
  (when (current-expression)
    (fprintf (current-error-port) "  at: ~a\n" (format-expression (current-expression)))))

;; ============================================================================
;; Compilation Errors
;; ============================================================================

(define (compile-error msg [expr #f])
  "Signal a compilation error with context"
  (let ([expr-to-show (or expr (current-expression))])
    (fprintf (current-error-port) "Compilation error: ~a\n" msg)
    (when expr-to-show
      (fprintf (current-error-port) "  at: ~a\n" (format-expression expr-to-show)))
    (show-error-context)
    (error 'compile "~a" msg)))

(define (syntax-error msg expr)
  "Signal a syntax error"
  (fprintf (current-error-port) "Syntax error: ~a\n" msg)
  (fprintf (current-error-port) "  at: ~a\n" (format-expression expr))
  (show-error-context)
  (error 'syntax "~a" msg))

(define (runtime-error msg expr)
  "Signal a runtime error (detected at compile time)"
  (fprintf (current-error-port) "Runtime error (detected at compile time): ~a\n" msg)
  (fprintf (current-error-port) "  at: ~a\n" (format-expression expr))
  (show-error-context)
  (error 'runtime "~a" msg))

;; ============================================================================
;; Specific Error Types
;; ============================================================================

(define (undefined-variable-error var)
  (compile-error (format "Undefined variable: ~a" var) var))

(define (invalid-syntax-error form expr)
  (syntax-error (format "Invalid syntax for ~a" form) expr))

(define (arity-error expected actual expr)
  (compile-error
   (format "Arity mismatch: expected ~a arguments, got ~a" expected actual)
   expr))

(define (type-error expected actual expr)
  (compile-error
   (format "Type error: expected ~a, got ~a" expected actual)
   expr))

(define (duplicate-definition-error name)
  (compile-error (format "Duplicate definition: ~a" name) name))

;; ============================================================================
;; Warning Messages
;; ============================================================================

(define show-warnings (make-parameter #t))

(define (warning msg [expr #f])
  "Show a warning message"
  (when (show-warnings)
    (fprintf (current-error-port) "Warning: ~a\n" msg)
    (when expr
      (fprintf (current-error-port) "  at: ~a\n" (format-expression expr)))
    (show-error-context)))

(define (unused-variable-warning var)
  (warning (format "Unused variable: ~a" var) var))

(define (dead-code-warning expr)
  (warning "Dead code: expression has no effect" expr))

(define (deprecated-warning feature alternative)
  (warning (format "~a is deprecated, use ~a instead" feature alternative)))
