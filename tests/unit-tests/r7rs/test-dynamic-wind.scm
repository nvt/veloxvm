;;; VeloxVM Unit Tests - dynamic-wind and parameterize
;;; The rewriter expands dynamic-wind into a guard-wrapped form so the
;;; after-thunk runs both on normal return and on exception unwinding.
;;; parameterize builds on dynamic-wind to give scoped parameter
;;; restoration. make-parameter is from the runtime library
;;; r7rs-parameters.scm.

(include "../unit-test-framework.scm")

;; Inline definition from r7rs-parameters.scm
(define (make-parameter init)
  (let ((value init))
    (lambda args
      (if (null? args)
          value
          (set! value (car args))))))

(test-suite "dynamic-wind and parameterize")

;; Order of execution on normal return
(define order '())
(define (push! x) (set! order (cons x order)))

(define dw-result
  (dynamic-wind
    (lambda () (push! 'before))
    (lambda () (push! 'thunk) 'thunk-value)
    (lambda () (push! 'after))))

(assert-equal '(after thunk before) order
              "before, thunk, after run in order on normal return")
(assert-equal 'thunk-value dw-result
              "dynamic-wind returns the thunk's result")

;; after-thunk runs on exception unwinding
(set! order '())
(define exc-result
  (guard (exc (else exc))
    (dynamic-wind
      (lambda () (push! 'before2))
      (lambda () (push! 'thunk2) (raise 'oops))
      (lambda () (push! 'after2)))))

(assert-equal '(after2 thunk2 before2) order
              "before, thunk, after still runs on exception")
(assert-equal 'oops exc-result
              "exception propagates to outer guard")

;; Nested dynamic-wind: inner-after runs before outer-after
(set! order '())
(dynamic-wind
  (lambda () (push! 'outer-before))
  (lambda ()
    (dynamic-wind
      (lambda () (push! 'inner-before))
      (lambda () (push! 'inner-thunk))
      (lambda () (push! 'inner-after))))
  (lambda () (push! 'outer-after)))

(assert-equal '(outer-after inner-after inner-thunk inner-before outer-before)
              order
              "nested dynamic-wind runs in correct order on normal return")

;; Nested + exception: inner-after then outer-after on exception
(set! order '())
(define nested-exc-result
  (guard (exc (else exc))
    (dynamic-wind
      (lambda () (push! 'outer-before))
      (lambda ()
        (dynamic-wind
          (lambda () (push! 'inner-before))
          (lambda () (raise 'boom))
          (lambda () (push! 'inner-after))))
      (lambda () (push! 'outer-after)))))

(assert-equal '(outer-after inner-after inner-before outer-before)
              order
              "nested dynamic-wind runs both afters on exception")
(assert-equal 'boom nested-exc-result "exception propagates through both")

;; parameterize: basic save and restore
(define p (make-parameter 10))
(assert-equal 10 (p) "parameter initial value")

(define inside-value #f)
(parameterize ((p 99))
  (set! inside-value (p)))

(assert-equal 99 inside-value "parameterize installed new value")
(assert-equal 10 (p) "parameter restored after exit")

;; parameterize: restore even when body raises
(define caught-with-param
  (guard (exc (else exc))
    (parameterize ((p 555))
      (raise 'param-error))))

(assert-equal 'param-error caught-with-param
              "exception in parameterize body is caught")
(assert-equal 10 (p)
              "parameter is restored even when body raises")

;; parameterize: nested parameterize stacks
(define p2 (make-parameter 'base))
(define inner-snapshot #f)
(define between-snapshot #f)

(parameterize ((p2 'outer))
  (set! inner-snapshot (p2))
  (parameterize ((p2 'inner))
    (set! between-snapshot (p2)))
  (set! inner-snapshot (p2)))

(assert-equal 'inner between-snapshot "innermost parameterize wins inside")
(assert-equal 'outer inner-snapshot "outer value seen between/after inner")
(assert-equal 'base (p2) "outermost restored to base")

;; parameterize: multiple parameters in one form
(define a (make-parameter 1))
(define b (make-parameter 2))
(define ab-product 0)
(parameterize ((a 10) (b 20))
  (set! ab-product (* (a) (b))))
(assert-equal 200 ab-product
              "multiple parameters install simultaneously")
(assert-equal 1 (a) "first parameter restored")
(assert-equal 2 (b) "second parameter restored")

(test-summary)
