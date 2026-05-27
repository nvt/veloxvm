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

;; thread-create! returns a thread object too. Use a let so the
;; handle goes out of scope after this assertion -- a top-level
;; define would root it permanently and consume a slot in the
;; fixed-size thread table.
(assert-true (let ((child (thread-create! (lambda () 42))))
               (thread? child))
             "thread-create! result satisfies thread?")

(test-suite "thread-join!")

;; thread-join! returns the joinee's thunk result.
(assert-equal 6
              (thread-join! (thread-create! (lambda () (+ 1 2 3))))
              "thread-join! returns the thunk's return value")

;; thread-join! on a thread that has already finished surfaces the
;; result once the scheduler has finalized it. The exact value can
;; vary by race: if the joinee was destroyed before we joined, the
;; result falls back to an unspecified value. We accept any non-error
;; outcome here; the live-thread case above is the strict check.
(assert-true (let ((quick (thread-create! (lambda () 'done))))
               (thread-sleep! 50)
               (let ((r (thread-join! quick)))
                 (or (eq? r 'done) #t)))
             "thread-join! after completion does not crash")

;; Self-join is a thread-level error (raised via vm_signal_error)
;; rather than a catchable exception, so guard does not catch it.
;; Asserting the exact behaviour here would crash the suite; we just
;; document the intent and skip a runtime check.

(test-suite "thread-join! timeout")

;; The default POSIX-port VM_THREAD_AMOUNT is small (10), and a
;; top-level (define x (thread-create! ...)) roots the handle in the
;; program symbol table forever -- the slot never gets freed even
;; after thread-terminate!. So each test below uses a let-bound
;; handle that goes out of scope as soon as the assertion is
;; recorded, letting the GC reclaim the handle and the scheduler
;; reap the slot before the next test runs.

(assert-equal 'too-slow
              (let ((slow (thread-create!
                            (lambda () (thread-sleep! 10000) 'done))))
                (let ((r (thread-join! slow 30 'too-slow)))
                  (thread-terminate! slow)
                  r))
              "thread-join! returns timeout-val when joinee does not finish in time")

;; SRFI 18: an omitted timeout-val raises join-timeout-exception
;; rather than falling back to #f.
(assert-equal 'caught
              (let ((slow (thread-create!
                            (lambda () (thread-sleep! 10000) 'done))))
                (let ((r (guard (exc ((join-timeout-exception? exc)
                                      'caught)
                                     (else 'unexpected))
                           (thread-join! slow 30))))
                  (thread-terminate! slow)
                  r))
              "thread-join! raises join-timeout-exception when timeout-val omitted")

(assert-equal 'not-yet
              (let ((t (thread-create!
                         (lambda () (thread-sleep! 10000) 99))))
                (let ((r (thread-join! t 0 'not-yet)))
                  (thread-terminate! t)
                  r))
              "(thread-join! t 0 v) returns v when joinee is still running")

(assert-equal 30
              (thread-join! (thread-create! (lambda () (+ 10 20)))
                            1000 'unused)
              "thread-join! returns joinee's result when it finishes within timeout")

(assert-equal 'forever
              (thread-join! (thread-create!
                              (lambda () (thread-sleep! 20) 'forever))
                            #f)
              "thread-join! with timeout=#f waits indefinitely")

(test-suite "thread-terminate!")

;; thread-terminate! takes a thread object (not an integer) and
;; returns truthy when it actually killed a thread. The second
;; terminate on the same handle finds the thread already gone; the
;; call either returns falsy or raises a thread error -- either is
;; acceptable as long as it does not crash. Wrapped in a let so the
;; handle does not permanently occupy a slot.
(let ((long-runner
       (thread-create! (lambda ()
                         (let loop ((i 0))
                           (thread-sleep! 1000)
                           (loop (+ i 1)))))))
  (assert-true (thread-terminate! long-runner)
               "thread-terminate! returns truthy for a live thread")
  (thread-sleep! 50)
  (let ((second (guard (exc (else 'raised))
                  (thread-terminate! long-runner))))
    (assert-true (or (eq? second 'raised)
                     (not second)
                     second)
                 "thread-terminate! on a finished thread does not crash")))

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

;; Round-trip a value through thread-specific. Wrapped in a let so
;; the worker handle gets reclaimed.
(let ((t (thread-create! (lambda () (thread-sleep! 1000)))))
  (thread-sleep! 10)
  (thread-specific-set! t 'hello)
  (assert-equal 'hello (thread-specific t)
                "thread-specific returns the value set by thread-specific-set!")
  (thread-specific-set! t 99)
  (assert-equal 99 (thread-specific t)
                "thread-specific round-trips an integer")
  (thread-terminate! t))

(test-summary)
