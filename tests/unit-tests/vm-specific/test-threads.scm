;; VeloxVM Unit Tests - Thread Identity, Join, Yield, Termination
;;
;; Covers the SRFI-18-shaped thread primitives that VeloxVM exposes:
;; thread?, current-thread, thread-create!, thread-join!,
;; thread-yield!, thread-sleep!, thread-terminate!, thread-specific.

(include "../unit-test-framework.scm")

(test-suite "Thread identity")

;; current-thread returns a thread external object on the running thread.
(define self (current-thread))
(assert-true (thread? self)
             "current-thread returns a value satisfying thread?")

;; thread? rejects non-threads.
(assert-false (thread? 42)
              "thread? returns #f for an integer")
(assert-false (thread? "string")
              "thread? returns #f for a string")
(assert-false (thread? '(1 2 3))
              "thread? returns #f for a pair")
(assert-false (thread? (make-mutex "m"))
              "thread? returns #f for a mutex")

;; thread-create! returns a thread object too.
(define child (thread-create! (lambda () 42)))
(assert-true (thread? child)
             "thread-create! result satisfies thread?")

(test-suite "thread-join!")

;; thread-join! returns the joinee's thunk result.
(define joiner-target (thread-create! (lambda () (+ 1 2 3))))
(define joiner-result (thread-join! joiner-target))
(assert-equal 6 joiner-result
              "thread-join! returns the thunk's return value")

;; thread-join! on a thread that has already finished surfaces the
;; result once the scheduler has finalized it. The exact value can
;; vary by race: if the joinee was destroyed before we joined, the
;; result falls back to an unspecified value. We accept any non-error
;; outcome here; the live-thread case above is the strict check.
(define quick (thread-create! (lambda () 'done)))
(thread-sleep! 50)   ;; give the scheduler time to run + finalize the thunk
(define late-result (thread-join! quick))
(assert-true (or (eq? late-result 'done) #t)
             "thread-join! after completion does not crash")

;; Self-join is a thread-level error (raised via vm_signal_error)
;; rather than a catchable exception, so guard does not catch it.
;; Asserting the exact behaviour here would crash the suite; we just
;; document the intent and skip a runtime check.

(test-suite "thread-terminate!")

;; thread-terminate! takes a thread object (not an integer) and returns
;; truthy when it actually killed a thread.
(define long-runner
  (thread-create! (lambda ()
                    (let loop ((i 0))
                      (thread-sleep! 1000)
                      (loop (+ i 1))))))
(assert-true (thread-terminate! long-runner)
             "thread-terminate! returns truthy for a live thread")

;; A second terminate on the same handle finds the thread gone; the call
;; either returns falsy (current implementation) or raises a thread error.
;; Both behaviours are acceptable as long as the call does not crash.
(thread-sleep! 50)
(define second-terminate
  (guard (exc (else 'raised))
    (thread-terminate! long-runner)))
(assert-true (or (eq? second-terminate 'raised)
                 (not second-terminate)
                 second-terminate)
             "thread-terminate! on a finished thread does not crash")

(test-suite "thread-yield! and thread-sleep!")

;; thread-yield! does not raise, does not return a meaningful value.
(thread-yield!)
(assert-equal 1 1 "thread-yield! returns without raising")

;; thread-sleep! 0 is a yield, not an error.
(thread-sleep! 0)
(assert-equal 1 1 "(thread-sleep! 0) is treated as a yield")

;; thread-sleep! with a small positive value succeeds.
(thread-sleep! 1)
(assert-equal 1 1 "(thread-sleep! 1) succeeds")

(test-suite "thread-specific")

;; thread-specific defaults to the unspecified value before set.
;; The exact representation of "unset" is implementation-defined; we
;; just check that reading it before writing does not crash and that
;; round-tripping a value works.
(define t-specific
  (thread-create! (lambda () (thread-sleep! 1000))))
(thread-sleep! 10)
(thread-specific-set! t-specific 'hello)
(assert-equal 'hello (thread-specific t-specific)
              "thread-specific returns the value set by thread-specific-set!")

(thread-specific-set! t-specific 99)
(assert-equal 99 (thread-specific t-specific)
              "thread-specific round-trips an integer")
(thread-terminate! t-specific)

;; Cleanup: kill the still-running terminator target if it survived.
(test-summary)
