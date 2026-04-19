;; VeloxVM Unit Tests - Exception Handling
;; Tests for: guard, raise

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Exception Handling")

;; Basic guard - catching raised exception
(assert-equal 'caught
  (guard (e
          ((eq? e 'test-error) 'caught)
          (else 'unknown))
    (raise 'test-error))
  "Guard catches raised exception")

;; Guard with else clause
(assert-equal 'unknown
  (guard (e
          ((eq? e 'specific-error) 'specific)
          (else 'unknown))
    (raise 'other-error))
  "Guard else clause catches unmatched exception")

;; Guard with no exception
(assert-equal 'normal-result
  (guard (e
          ((eq? e 'error) 'caught)
          (else 'unknown))
    'normal-result)
  "Guard returns body value when no exception")

;; Division by zero exception
(assert-equal 'div-zero-caught
  (guard (obj
          ((eq? obj 'Div0Exception) 'div-zero-caught)
          (else 'other-error))
    (/ 5 0))
  "Division by zero raises Div0Exception")

;; Nested guard
(assert-equal 'inner-caught
  (guard (outer
          ((eq? outer 'outer-error) 'outer-caught)
          (else 'outer-unknown))
    (guard (inner
            ((eq? inner 'inner-error) 'inner-caught)
            (else 'inner-unknown))
      (raise 'inner-error)))
  "Inner guard catches exception")

;; Exception propagation to outer guard
(assert-equal 'outer-caught
  (guard (outer
          ((eq? outer 'propagated) 'outer-caught)
          (else 'outer-unknown))
    (guard (inner
            ((eq? inner 'inner-specific) 'inner-caught)
            (else (raise inner)))
      (raise 'propagated)))
  "Exception propagates to outer guard when inner doesn't handle it")

;; Guard with computation before exception
(assert-equal 'computed-and-caught
  (guard (e
          ((eq? e 'after-compute) 'computed-and-caught)
          (else 'other))
    (define x 5)
    (define y (* x 2))
    (when (= y 10)
      (raise 'after-compute))
    'not-raised)
  "Guard catches exception after computation")

;; Multiple condition tests in guard
(assert-equal 'type-a
  (guard (e
          ((eq? e 'type-a) 'type-a)
          ((eq? e 'type-b) 'type-b)
          ((eq? e 'type-c) 'type-c)
          (else 'other))
    (raise 'type-a))
  "Guard with multiple specific handlers - first")

(assert-equal 'type-b
  (guard (e
          ((eq? e 'type-a) 'type-a)
          ((eq? e 'type-b) 'type-b)
          ((eq? e 'type-c) 'type-c)
          (else 'other))
    (raise 'type-b))
  "Guard with multiple specific handlers - second")

(assert-equal 'type-c
  (guard (e
          ((eq? e 'type-a) 'type-a)
          ((eq? e 'type-b) 'type-b)
          ((eq? e 'type-c) 'type-c)
          (else 'other))
    (raise 'type-c))
  "Guard with multiple specific handlers - third")

;; Guard returning computed value from handler
(assert-equal 100
  (guard (e
          ((number? e) (* e 10))
          (else 0))
    (raise 10))
  "Guard handler can compute return value")

;; Guard with string exception
(assert-equal "error-message"
  (guard (e
          ((string? e) e)
          (else "unknown"))
    (raise "error-message"))
  "Guard can catch string exception")

;; Guard with list exception (structured error)
(assert-equal '(error file-not-found "config.txt")
  (guard (e
          ((and (list? e) (eq? (car e) 'error)) e)
          (else 'unknown))
    (raise '(error file-not-found "config.txt")))
  "Guard can catch structured error list")

;; Accessing exception data
(assert-equal "config.txt"
  (guard (e
          ((and (list? e) (eq? (car e) 'error))
           (list-ref e 2))
          (else 'unknown))
    (raise '(error file-not-found "config.txt")))
  "Handler can extract data from exception")

(test-suite "Exception - Practical Patterns")

;; Safe division function
(define (safe-divide a b)
  (guard (e
          ((eq? e 'Div0Exception) 'infinity)
          (else 'error))
    (/ a b)))

(assert-equal 5 (safe-divide 10 2) "Safe divide normal case")
(assert-equal 'infinity (safe-divide 10 0) "Safe divide by zero")

;; Try-catch-finally pattern simulation
(define finally-executed #f)
(define (with-cleanup body cleanup)
  (let ((result
         (guard (e
                 (else
                  (cleanup)
                  (raise e)))
           (let ((r (body)))
             (cleanup)
             r))))
    result))

;; Test that cleanup runs on success
(set! finally-executed #f)
(assert-equal 'success
  (guard (e (else 'error))
    (with-cleanup
     (lambda () 'success)
     (lambda () (set! finally-executed #t))))
  "With-cleanup returns body result")
(assert-true finally-executed "Cleanup executed on success")

(test-suite "Exception - Automatic Re-raising (R6RS/R7RS)")

;; Basic auto re-raise: guard without else clause re-raises to outer guard
(assert-equal 'outer-caught
  (guard (outer
          ((eq? outer 'unhandled) 'outer-caught))
    (guard (inner
            ((eq? inner 'specific) 'inner-caught))
      (raise 'unhandled)))
  "Auto re-raise when no else clause")

;; Multiple levels of auto re-raising
(assert-equal 'outermost
  (guard (outer1
          ((eq? outer1 'error) 'outermost))
    (guard (outer2
            ((eq? outer2 'foo) 'middle))
      (guard (inner
              ((eq? inner 'bar) 'innermost))
        (raise 'error))))
  "Auto re-raise through multiple guard levels")

;; Auto re-raise with some clauses matching at different levels
(assert-equal 'middle-caught
  (guard (outer
          ((eq? outer 'outer-error) 'outer-caught))
    (guard (middle
            ((eq? middle 'propagated) 'middle-caught))
      (guard (inner
              ((eq? inner 'inner-error) 'inner-caught))
        (raise 'propagated))))
  "Auto re-raise until matching clause found")

;; Explicit else clause takes precedence over auto re-raise
(assert-equal 'explicit-handled
  (guard (outer
          ((eq? outer 'other) 'outer-should-not-match))
    (guard (inner
            ((eq? inner 'foo) 'matched)
            (else 'explicit-handled))
      (raise 'other)))
  "Explicit else clause prevents auto re-raise")

;; Unhandled exception at top level (no outer guard)
(assert-equal 'top-level-handled
  (guard (e
          ((eq? e 'final) 'top-level-handled))
    (guard (inner
            ((eq? inner 'nope) 'inner-match))
      (raise 'final)))
  "Auto re-raise to top-level guard")

;; Auto re-raise with complex condition tests
(assert-equal 'number-caught
  (guard (outer
          ((number? outer) 'number-caught)
          (else 'other))
    (guard (inner
            ((string? inner) 'string-caught))
      (raise 42)))
  "Auto re-raise with predicate-based clauses")

(test-summary)
