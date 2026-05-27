;; VeloxVM Unit Tests - Condition Variables (SRFI 18)
;;
;; Covers condition-variable?, make-condition-variable,
;; condition-variable-name, condition-variable-signal!,
;; condition-variable-broadcast!, and the timeout/cv extensions of
;; mutex-lock! / mutex-unlock!.

(include "../unit-test-framework.scm")

(test-suite "Condition variable identity")

(define cv1 (make-condition-variable "cv1"))
(assert-true (condition-variable? cv1)
             "make-condition-variable produces a value satisfying condition-variable?")
(assert-false (condition-variable? 42)
              "condition-variable? returns #f for an integer")
(assert-false (condition-variable? (make-mutex "m1"))
              "condition-variable? returns #f for a mutex")
(assert-false (condition-variable? (current-thread))
              "condition-variable? returns #f for a thread")
(assert-equal "cv1" (condition-variable-name cv1)
              "condition-variable-name round-trips through make-condition-variable")

(test-suite "Signal wakes one waiter")

;; A signaled waiter returns #t from (mutex-unlock! m cv).
(define m-sig (make-mutex "m-sig"))
(define cv-sig (make-condition-variable "cv-sig"))
(define result-sig #f)

(define t-sig
  (thread-create!
   (lambda ()
     (mutex-lock! m-sig)
     (let ((r (mutex-unlock! m-sig cv-sig)))
       (set! result-sig r)
       r))))

(thread-sleep! 30)            ;; let the waiter park on cv-sig
(condition-variable-signal! cv-sig)
(assert-equal #t (thread-join! t-sig)
              "mutex-unlock! m cv returns #t when signaled")
(assert-equal #t result-sig
              "the value bound inside the waiter matches the join result")

(test-suite "Timeout returns #f")

(define m-to (make-mutex "m-to"))
(define cv-to (make-condition-variable "cv-to"))

(define t-to
  (thread-create!
   (lambda ()
     (mutex-lock! m-to)
     (mutex-unlock! m-to cv-to 30))))  ;; 30 ms timeout, no signaler

(assert-equal #f (thread-join! t-to)
              "mutex-unlock! m cv timeout returns #f after the deadline")

(test-suite "Broadcast wakes all waiters")

;; Three workers all park on the same cv; broadcast wakes every one.
;; Each worker records its outcome in a per-worker box so we can
;; verify all three saw #t (not just one).
(define m-bc (make-mutex "m-bc"))
(define cv-bc (make-condition-variable "cv-bc"))
(define out1 (box #f))
(define out2 (box #f))
(define out3 (box #f))

(define (spawn-waiter out)
  (thread-create!
   (lambda ()
     (mutex-lock! m-bc)
     (box-set! out (mutex-unlock! m-bc cv-bc)))))

(define w1 (spawn-waiter out1))
(define w2 (spawn-waiter out2))
(define w3 (spawn-waiter out3))

(thread-sleep! 50)              ;; let all three park
(condition-variable-broadcast! cv-bc)

(thread-join! w1)
(thread-join! w2)
(thread-join! w3)
(assert-equal #t (box-ref out1) "broadcast wakes waiter 1")
(assert-equal #t (box-ref out2) "broadcast wakes waiter 2")
(assert-equal #t (box-ref out3) "broadcast wakes waiter 3")

(test-suite "mutex-lock! with timeout")

(define m-lt (make-mutex "m-lt"))
(mutex-lock! m-lt)              ;; main holds it; no one will release

;; Contended lock with timeout returns #f.
(define t-lt
  (thread-create!
   (lambda ()
     (mutex-lock! m-lt 30))))
(assert-equal #f (thread-join! t-lt)
              "mutex-lock! m timeout returns #f when never released")

;; Contended lock that does get released returns #t.
(define t-lt-ok
  (thread-create!
   (lambda ()
     (thread-sleep! 10)         ;; ensure main is past the unlock first
     (mutex-lock! m-lt 200))))  ;; should acquire after main unlocks
(thread-sleep! 5)
(mutex-unlock! m-lt)
(assert-equal #t (thread-join! t-lt-ok)
              "mutex-lock! m timeout returns #t when released in time")
(mutex-unlock! m-lt)            ;; release for cleanup

(test-suite "Signal with no waiters is a no-op")

(define cv-empty (make-condition-variable "empty"))
(condition-variable-signal! cv-empty)        ;; no waiters; no crash
(condition-variable-broadcast! cv-empty)     ;; same
(assert-equal 1 1
              "signal/broadcast on an empty cv does not raise")

(test-suite "condition-variable-specific")

(define cv-spec (make-condition-variable "spec"))
(condition-variable-specific-set! cv-spec 'hello)
(assert-equal 'hello (condition-variable-specific cv-spec)
              "condition-variable-specific round-trips a symbol")
(condition-variable-specific-set! cv-spec 42)
(assert-equal 42 (condition-variable-specific cv-spec)
              "condition-variable-specific round-trips an integer")
(condition-variable-specific-set! cv-spec '(a b c))
(assert-equal '(a b c) (condition-variable-specific cv-spec)
              "condition-variable-specific round-trips a list")

(test-suite "Optional names")

;; make-mutex without a name is allowed; mutex-name returns the empty
;; string.
(define m-anon (make-mutex))
(assert-true (mutex? m-anon)
             "(make-mutex) with no name produces a mutex")
(assert-equal "" (mutex-name m-anon)
              "anonymous mutex has empty-string name")

;; make-condition-variable without a name was already allowed; just
;; verify it still works after the specific-cell addition.
(define cv-anon (make-condition-variable))
(assert-true (condition-variable? cv-anon)
             "(make-condition-variable) with no name produces a cv")

(test-summary)
