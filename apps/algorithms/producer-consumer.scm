;; Bounded-buffer producer/consumer demo for VeloxVM SRFI-18 CVs.
;;
;; One producer fills a bounded FIFO queue with integers 1..N. Two
;; consumers pull items concurrently and accumulate them into a
;; shared sum under a mutex. Two condition variables coordinate the
;; queue: `not-full` lets the producer block when the buffer is at
;; capacity, `not-empty` lets consumers block when the queue is
;; empty. The producer broadcasts on `not-empty` after setting a
;; shutdown flag so consumers can wake and exit even with an empty
;; queue.
;;
;; Self-validating: the observed sum is compared against the
;; arithmetic-series sum of 1..N. Verifies that (mutex-unlock! m cv)
;; really is atomic with respect to the predicate retest -- without
;; that, a missed wakeup would produce a sum smaller than expected.

(define max-size 5)
(define item-count 50)
(define expected-sum (quotient (* item-count (+ item-count 1)) 2))

(define m (make-mutex "queue"))
(define not-full (make-condition-variable "not-full"))
(define not-empty (make-condition-variable "not-empty"))

;; Shared state, accessed only while m is held.
(define queue '())
(define queue-size 0)
(define done #f)
(define sum 0)

;; Producer: push item onto the FIFO. Blocks while the queue is at
;; capacity, then re-checks the predicate on each wakeup -- the
;; standard "loop around the wait" pattern that mutex-unlock!'s
;; atomic release-and-wait makes safe.
(define (produce item)
  (mutex-lock! m)
  (let loop ()
    (if (>= queue-size max-size)
        (begin (mutex-unlock! m not-full)
               (mutex-lock! m)
               (loop))))
  ;; append-at-tail keeps the FIFO ordering; max-size is small, so
  ;; the O(n) append cost is negligible.
  (set! queue (append queue (list item)))
  (set! queue-size (+ queue-size 1))
  (condition-variable-signal! not-empty)
  (mutex-unlock! m))

;; Consumer: pop one item, or return #f when the producer has
;; finished and the queue has drained.
(define (consume)
  (mutex-lock! m)
  (let loop ()
    (cond
      ((> queue-size 0)
       (let ((item (car queue)))
         (set! queue (cdr queue))
         (set! queue-size (- queue-size 1))
         (condition-variable-signal! not-full)
         (mutex-unlock! m)
         item))
      (done
       ;; queue empty AND producer signalled completion
       (mutex-unlock! m)
       #f)
      (else
       (mutex-unlock! m not-empty)
       (mutex-lock! m)
       (loop)))))

;; A consumer worker drains the queue into the running sum until it
;; sees #f (shutdown). The thread-yield! between items hands the
;; remainder of the per-invocation slice to the next thread so both
;; consumers actually share work; without it, whichever consumer
;; grabs the mutex first ends up doing all of it.
(define (consumer-worker)
  (let loop ((local-count 0))
    (let ((item (consume)))
      (if item
          (begin
            (mutex-lock! m)
            (set! sum (+ sum item))
            (mutex-unlock! m)
            (thread-yield!)
            (loop (+ local-count 1)))
          local-count))))

(define producer
  (thread-create!
   (lambda ()
     (let loop ((i 1))
       (if (> i item-count)
           (begin
             ;; Signal shutdown. Broadcast (not signal) because both
             ;; consumers may be parked on not-empty -- a single
             ;; signal would wake only one of them.
             (mutex-lock! m)
             (set! done #t)
             (condition-variable-broadcast! not-empty)
             (mutex-unlock! m)
             item-count)
           (begin (produce i)
                  (loop (+ i 1))))))))

(define c1 (thread-create! (lambda () (consumer-worker))))
(define c2 (thread-create! (lambda () (consumer-worker))))

(define produced (thread-join! producer))
(define count1 (thread-join! c1))
(define count2 (thread-join! c2))

(print "Producer/consumer demo\n")
(print "  buffer size:    ") (print max-size) (print "\n")
(print "  items produced: ") (print produced) (print "\n")
(print "  consumer 1 took: ") (print count1) (print "\n")
(print "  consumer 2 took: ") (print count2) (print "\n")
(print "  expected sum:   ") (print expected-sum) (print "\n")
(print "  observed sum:   ") (print sum) (print "\n")

(if (and (= sum expected-sum)
         (= (+ count1 count2) item-count))
    (print "PASS: producer/consumer round-trip matches expected\n")
    (print "FAIL: sum or count mismatch\n"))
