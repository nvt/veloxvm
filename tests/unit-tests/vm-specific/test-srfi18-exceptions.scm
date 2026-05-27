;; VeloxVM Unit Tests - SRFI 18 typed exceptions
;;
;; Covers the four runtime-raised condition types and the wrapped-
;; reason accessor:
;;
;;   join-timeout-exception?         (thread-join! timeout without timeout-val)
;;   abandoned-mutex-exception?      (mutex-lock! on an abandoned mutex)
;;   terminated-thread-exception?    (thread-join! on a terminated joinee)
;;   uncaught-exception?             (thread-join! on a joinee that died with a raise)
;;   uncaught-exception-reason       (unwrap the wrapped condition)
;;
;; All five primitives are spec-compliant SRFI 18 predicates; the
;; runtime raises the appropriate kind from the corresponding code
;; path. Tests use guard to dispatch on each predicate.

(include "../unit-test-framework.scm")

(test-suite "Predicate basics")

;; The predicates reject non-exception values cleanly.
(assert-false (join-timeout-exception? 42)
              "join-timeout-exception? returns #f for an integer")
(assert-false (abandoned-mutex-exception? "x")
              "abandoned-mutex-exception? returns #f for a string")
(assert-false (terminated-thread-exception? '(a b))
              "terminated-thread-exception? returns #f for a pair")
(assert-false (uncaught-exception? (current-thread))
              "uncaught-exception? returns #f for a thread")

(test-suite "join-timeout-exception")

(assert-equal 'timed-out
              (let ((slow (thread-create!
                            (lambda () (thread-sleep! 10000) 'done))))
                (let ((r (guard (exc ((join-timeout-exception? exc)
                                      'timed-out)
                                     (else 'other))
                           (thread-join! slow 30))))
                  (thread-terminate! slow)
                  r))
              "thread-join! with no timeout-val raises join-timeout-exception on timeout")

;; Supplying timeout-val suppresses the raise (back-compat shape).
(assert-equal 'fallback
              (let ((slow (thread-create!
                            (lambda () (thread-sleep! 10000) 'done))))
                (let ((r (thread-join! slow 30 'fallback)))
                  (thread-terminate! slow)
                  r))
              "thread-join! with timeout-val returns it on timeout, no raise")

(test-suite "terminated-thread-exception")

;; Joining a thread that was killed via thread-terminate! raises
;; terminated-thread-exception. We terminate first, then sleep a
;; little to let the scheduler finalize, then join.
(assert-equal 'killed
              (let ((victim (thread-create!
                              (lambda () (thread-sleep! 10000)))))
                (thread-terminate! victim)
                (thread-sleep! 30)
                (guard (exc ((terminated-thread-exception? exc)
                             'killed)
                            (else 'other))
                  (thread-join! victim)))
              "thread-join! on a terminated thread raises terminated-thread-exception")

(test-suite "uncaught-exception + reason")

;; A joinee that raises and does not catch it should surface as
;; uncaught-exception in the joiner. The reason is the original
;; value the joinee raised.
(assert-equal 'inner-payload
              (let ((blowup
                     (thread-create!
                       (lambda () (raise 'inner-payload)))))
                (thread-sleep! 30)  ;; let the joinee die
                (guard (exc ((uncaught-exception? exc)
                             (uncaught-exception-reason exc))
                            (else 'unexpected))
                  (thread-join! blowup)))
              "uncaught-exception-reason returns the original raised value")

;; uncaught-exception-reason on a non-uncaught condition raises a
;; type error -- but it is a thread-level signal_error, not a
;; guard-catchable exception, so testing the negative case here
;; would crash the suite. The positive case above is the load-
;; bearing assertion.

(test-suite "abandoned-mutex-exception")

;; SRFI 18: if a mutex's owner dies without unlocking, the next
;; locker observes abandoned-mutex-exception. Our implementation
;; only flags MUTEX_ABANDONED when an owner thread is terminated
;; while holding the mutex; current threading primitives do not
;; expose mutex ownership tracking deeply enough to set that flag
;; from the kill path. We test the predicate's negative case and
;; document the positive case as not-yet-wired.
(let ((m (make-mutex "abandoned-test")))
  (mutex-lock! m)
  (mutex-unlock! m)
  (assert-equal 'normal
                (guard (exc ((abandoned-mutex-exception? exc)
                             'abandoned)
                            (else 'other))
                  (mutex-lock! m)
                  'normal)
                "mutex-lock! on a normally-released mutex does not raise abandoned-mutex-exception")
  (mutex-unlock! m))

(test-summary)
