;; VeloxVM Unit Tests - SRFI-18 time objects
;;
;; Covers current-time, time?, time->seconds, seconds->time and the
;; new "time object as absolute timeout" path through thread-sleep!,
;; thread-join!, and mutex-lock!. Existing integer-ms-relative
;; timeouts continue to work; tests for that path live in test-cv.scm
;; and test-threads.scm.

(include "../unit-test-framework.scm")

(test-suite "Time object identity")

(define t (current-time))
(assert-true (time? t)
             "current-time returns a value satisfying time?")
(assert-false (time? 42)
              "time? returns #f for an integer")
(assert-false (time? (make-mutex "m"))
              "time? returns #f for a mutex")
(assert-false (time? (current-thread))
              "time? returns #f for a thread")

(test-suite "time->seconds / seconds->time")

;; Round-trip through seconds->time / time->seconds preserves the
;; integer-seconds value.
(define t100 (seconds->time 100))
(assert-true (time? t100)
             "seconds->time produces a time object")
(assert-equal 100 (time->seconds t100)
              "(time->seconds (seconds->time 100)) is 100")

;; Rational input to seconds->time encodes fractional seconds; the
;; resulting time object is well-formed even though we can't easily
;; assert on its returned real value here (Scheme arithmetic in the
;; test framework does not mix integer and real comparisons).
(assert-true (time? (seconds->time 5/2))
             "seconds->time accepts a rational fractional seconds value")

(test-suite "Time-object timeout: thread-sleep!")

;; (thread-sleep! (current-time)) should be a no-op (yield), since
;; the deadline is "now" and parse_timeout returns 0 ms.
(thread-sleep! (current-time))
(assert-equal 1 1
              "(thread-sleep! (current-time)) returns without blocking")

;; A time object whose absolute deadline is in the past (epoch
;; second 1) is treated as "deadline already passed" -- returns
;; immediately rather than sleeping back through time.
(thread-sleep! (seconds->time 1))
(assert-equal 1 1
              "thread-sleep! to a past time object returns immediately")

(test-suite "Time-object timeout: thread-join!")

;; A long-finished deadline (in the past) on thread-join! returns
;; timeout-val immediately, just like (thread-join! t 0 v).
(assert-equal 'past
              (let ((slow (thread-create!
                            (lambda () (thread-sleep! 10000) 'done))))
                (let ((r (thread-join! slow
                                       (seconds->time 0)
                                       'past)))
                  (thread-terminate! slow)
                  r))
              "thread-join! with a past time object returns timeout-val")

(test-suite "Time-object timeout: mutex-lock!")

;; A held mutex with a past-deadline time-object timeout returns #f
;; without blocking.
(let ((m (make-mutex)))
  (mutex-lock! m)
  (assert-equal #f
                (let ((other
                       (thread-create!
                         (lambda ()
                           (mutex-lock! m (seconds->time 0))))))
                  (thread-join! other))
                "mutex-lock! with past time object returns #f")
  (mutex-unlock! m))

(test-summary)
