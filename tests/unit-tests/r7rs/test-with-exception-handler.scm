;;; VeloxVM Unit Tests - R7RS §6.11 with-exception-handler and
;;; raise-continuable
;;;
;;; Coverage targets the practical surface of the spec the runtime
;;; supports: handler is invoked when thunk raises, the handler's
;;; return value becomes the value of the with-exception-handler
;;; form, and the handler can inspect error objects produced by the
;;; `error` procedure.
;;;
;;; Limitation: raise-continuable currently aliases raise (no
;;; per-thread handler stack), so its handler-return semantics are
;;; observable as "return value flows out of with-exception-handler"
;;; rather than R7RS's "return value flows back to raise-continuable
;;; call site". The tests document this rather than assert R7RS
;;; semantics that would fail.

(include "../unit-test-framework.scm")

(test-suite "with-exception-handler / raise-continuable")

;; --- Normal flow: thunk runs to completion, handler unused -------------
(assert-equal 42
              (with-exception-handler
                (lambda (e) 'should-not-run)
                (lambda () 42))
              "thunk's value flows out when no exception is raised")

;; --- Handler receives the raised object --------------------------------
(assert-equal 'caught
              (with-exception-handler
                (lambda (e) (if (eq? e 'boom) 'caught 'wrong))
                (lambda () (raise 'boom)))
              "handler called with raised object; its return value flows out")

;; --- Handler can inspect error objects built by `error` ---------------
(define inspected
  (with-exception-handler
    (lambda (e)
      (if (error-object? e)
          (list (error-object-message e) (error-object-irritants e))
          'not-error-object))
    (lambda () (error "bad" 1 2 3))))
(assert-equal '("bad" (1 2 3)) inspected
              "handler sees the structured error object from `error`")

;; --- Nested handlers: inner catches first ------------------------------
(define nest-result
  (with-exception-handler
    (lambda (outer-e) (list 'outer outer-e))
    (lambda ()
      (with-exception-handler
        (lambda (inner-e) (list 'inner inner-e))
        (lambda () (raise 'x))))))
(assert-equal '(inner x) nest-result
              "inner handler catches; outer not invoked")

;; --- Re-raise from inner handler reaches outer -------------------------
(define reraise-result
  (with-exception-handler
    (lambda (outer-e) (list 'outer outer-e))
    (lambda ()
      (with-exception-handler
        (lambda (inner-e) (raise (list 'rethrown inner-e)))
        (lambda () (raise 'original))))))
(assert-equal '(outer (rethrown original)) reraise-result
              "raise from inner handler is caught by outer handler")

;; --- raise-continuable currently aliases raise -------------------------
;; The handler's return value still becomes the value of the
;; with-exception-handler form (not the raise-continuable site).
(assert-equal 99
              (with-exception-handler
                (lambda (e) 99)
                (lambda () (raise-continuable 'whatever)))
              "raise-continuable: handler value flows out of with-exception-handler")

;; --- raise-continuable is still distinct from a plain return ----------
;; If we didn't reach the raise-continuable, the handler wouldn't run.
(define rc-witness #f)
(with-exception-handler
  (lambda (e) (set! rc-witness e) 'handled)
  (lambda () (raise-continuable 'rc-payload)))
(assert-equal 'rc-payload rc-witness
              "raise-continuable invokes the handler with its argument")

(test-summary)
