;; A long-running heartbeat app for exercising the Contiki-NG VM shell
;; commands (vm-ps, vm-mem, vm-perf). Each tick wakes once per second,
;; runs a small calculation, and prints a status line.

(define tick-interval 1000)

;; Iterative sum 1 + 2 + ... + n. Cheap but allocates fixnums each
;; pass, so vm-mem allocation counters move.
(define triangular
  (lambda (n)
    (let loop ((i 1) (acc 0))
      (if (> i n)
          acc
          (loop (+ i 1) (+ acc i))))))

;; Iterative Fibonacci. The recursive form blows VM_CONTEXT_STACK_SIZE
;; on the default Contiki-NG build; the loop form keeps stack depth
;; constant while still doing real work each tick.
(define fib
  (lambda (n)
    (let loop ((i 0) (a 0) (b 1))
      (if (>= i n)
          a
          (loop (+ i 1) b (+ a b))))))

(define heartbeat
  (lambda (count)
    (print "tick " count
           " tri=" (triangular 100)
           " fib30=" (fib 30)
           #\Newline)
    (thread-sleep! tick-interval)
    (heartbeat (+ count 1))))

(heartbeat 1)
