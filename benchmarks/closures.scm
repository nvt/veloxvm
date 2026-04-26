;; Closure creation and invocation throughput.
;;
;; Builds N independent counter closures, then increments each one M
;; times. Each call exercises:
;;   - closure dispatch (scheduler -> closure->form_id -> bind_function)
;;   - captures binding (the c name is bound to the closure's heap box)
;;   - box-ref and box-set! through the box rewrite
;;
;; Closure creation is exercised once per counter; box ops fire on every
;; call. Use this as a regression / performance probe for the closure
;; runtime and for the box primitive throughput.

(print "=== Closures ===\n")

(define (make-counter)
  (let ((c 0))
    (lambda ()
      (set! c (+ c 1))
      c)))

(define n-counters 100)
(define n-increments 1000)

(define (build-counters n acc)
  (if (= n 0) acc (build-counters (- n 1) (cons (make-counter) acc))))

(define counters (build-counters n-counters '()))

(define (call-many ctr k)
  (if (= k 0) 'done (begin (ctr) (call-many ctr (- k 1)))))

(define (increment-each lst k)
  (if (null? lst)
      'done
      (begin (call-many (car lst) k) (increment-each (cdr lst) k))))

(print "create + call: ") (print n-counters) (print " counters x ")
(print n-increments) (print " calls each\n")
(increment-each counters n-increments)

;; Sanity check: every counter should now read its own n-increments.
(define (read-counter ctr) (ctr))
(print "  first counter final value: ")
(print (read-counter (car counters)))
(print " (expected ")
(print (+ n-increments 1))
(print ")\n=== Done ===\n")
