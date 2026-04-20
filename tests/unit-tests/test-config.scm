;; Test Framework - Configuration
;; Centralized configuration for test framework behavior

;; Verbosity levels
(define *test-verbose* #t)   ; Show individual test results (PASS/FAIL/SKIP)
(define *test-quiet* #f)     ; Only show summary (overrides verbose)
(define *test-show-passed* #t) ; Show passed tests (only if verbose)

;; Test context stack for better error messages
;; Format: (("Context 1") ("Context 2" "Subcontext") ...)
(define *test-context-stack* '())

;; Helper: Get current context as string
(define (get-test-context)
  (if (null? *test-context-stack*)
      ""
      (let ((contexts (reverse *test-context-stack*)))
        (string-append "["
                       (let loop ((ctx contexts))
                         (cond
                           ((null? ctx) "")
                           ((null? (cdr ctx)) (car ctx))
                           (else (string-append (car ctx) " → " (loop (cdr ctx))))))
                       "] "))))

;; Push context onto stack
(define (push-test-context context)
  (set! *test-context-stack* (cons context *test-context-stack*)))

;; Pop context from stack
(define (pop-test-context)
  (if (not (null? *test-context-stack*))
      (set! *test-context-stack* (cdr *test-context-stack*))
      #f))

;; Macro: Execute code with test context
;; Temporarily disabled for debugging
;; (define-syntax with-test-context
;;   (syntax-rules ()
;;     ((with-test-context context body ...)
;;      (begin
;;        (push-test-context context)
;;        body ...
;;        (pop-test-context)))))

