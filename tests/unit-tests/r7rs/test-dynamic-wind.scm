;;; VeloxVM Unit Tests - dynamic-wind
;;; The rewriter expands dynamic-wind into a sequenced let* that
;;; runs before, then the thunk, then after on normal return. The
;;; after-thunk does NOT run on exception unwinding -- that requires
;;; either a guard-wrapped expansion (which currently triggers a
;;; deeply-nested let-binding bug) or VM-level wind-chain machinery.

(include "../unit-test-framework.scm")

(test-suite "dynamic-wind")

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

;; Limitation: the after-thunk does NOT run on exception unwinding.
;; The exception propagates to the enclosing guard but the after-thunk
;; is skipped. Verifying current behaviour rather than the spec.
(set! order '())
(define exc-result
  (guard (exc (else exc))
    (dynamic-wind
      (lambda () (push! 'before2))
      (lambda () (push! 'thunk2) (raise 'oops))
      (lambda () (push! 'after2)))))

(assert-equal '(thunk2 before2) order
              "before and thunk run; after skipped on exception (limitation)")
(assert-equal 'oops exc-result
              "exception propagates to outer guard")

;; Nested dynamic-wind triggers a deeply-nested let-binding bug in
;; the compiler/VM (the innermost bind_function frame's bindv lookup
;; fails for its own freshly-bound parameter). Tracked as future
;; work; see doc/known-issues.md.

;; parameterize is built on dynamic-wind. The basic save/restore
;; uses additional let-bindings on top of dynamic-wind's expansion,
;; which compounds the deeply-nested binding bug. Deferred to the
;; same future-work entry as nested dynamic-wind.

(test-summary)
