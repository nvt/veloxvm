;; Naive recursive Fibonacci, repeated.
;;
;; (fib n) makes 2*fib(n+1)-1 calls and recurses to depth n. With
;; VM_CONTEXT_STACK_SIZE = 64 (POSIX) and ~4 frames per recursive
;; level, the cap is around fib(15) for a bare call -- with the outer
;; tail-recursive wrapper consuming ~4 frames as well, n = 12 leaves
;; comfortable headroom. We compensate with a high repeat count.
;;
;; What this probes:
;;   * call frame push/pop in vm-sched.c
;;   * symbol resolution for n on every call
;;   * arithmetic primitive dispatch (<, +, -)
;;   * outer-loop TCO interaction with non-tail inner recursion
;;
;; Cross-frontend: same source structure compiles under Cyclus and
;; Python; comparing emitted bytecode/runtime gives a frontend
;; quality measurement for free.

(print "=== Fibonacci (recursive, repeated) ===\n")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define n 12)
(define repeats 1000)
(define expected 144)  ; fib(12)
(define calls-per-fib 465)  ; 2*fib(13)-1

(define (loop k last)
  (if (= k 0) last (loop (- k 1) (fib n))))

(print "Computing (fib ") (print n) (print ") x ") (print repeats)
(print " times...\n")

(define t-start (time))
(define result (loop repeats 0))
(define t-end (time))

(print "  result:   ") (print result) (print "\n")
(print "  expected: ") (print expected) (print "\n")
(print "  total calls: ~") (print (* repeats calls-per-fib)) (print "\n")
(print "  elapsed:  ") (print (- t-end t-start)) (print " ms\n")

(if (= result expected)
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
