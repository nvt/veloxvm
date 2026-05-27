;; Allocate-and-drop floor benchmark.
;;
;; Tight loop that allocates short-lived cons cells with no retained
;; structure. Live set stays near zero; only the pressure loop's own
;; cons cells are live for one instruction at a time. This isolates
;; the GC's constant-overhead path: the always-walked roots, the
;; allocation hash table scan over allocations.size (vm-memory.c:555),
;; the pool ref-bitmap clear (vm-mempool.c:294-309), and the finalize-
;; list head reads.
;;
;; Useful as a floor measurement: how much time is spent in GC
;; plumbing even when there is essentially nothing to mark and nothing
;; to sweep? Subtract from richer benchmarks to isolate live-set cost.

(print "=== Allocate-and-drop floor ===\n")

(define iters 200000)

(define (alloc-drop k)
  (if (= k 0) 'done
      (begin
        (cons k '())
        (cons k k)
        (cons k (cons k '()))
        (alloc-drop (- k 1)))))

(print "Running ") (print iters)
(print " iterations of cons-and-drop (3 allocs each, ")
(print (* iters 3)) (print " cells total)...\n")

(define t-start (time))
(alloc-drop iters)
(define t-end (time))
(define elapsed (- t-end t-start))
(define total-allocations (* iters 3))

(print "  elapsed: ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:    ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(print "Status: PASS\n")
