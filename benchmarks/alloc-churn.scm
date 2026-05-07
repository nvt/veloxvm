;; Sustained allocation churn against a top-level data list.
;;
;; Workload per iteration (results discarded by begin, so each
;; iteration's intermediate lists become unreachable before the
;; next allocation):
;;
;;   * (map (lambda (x) (* x 2)) data)   - allocates 200 cons cells
;;   * (filter even? data)                - allocates ~100 cells
;;
;; A final (reduce + data) checks data is still intact. At 1000
;; iters the benchmark allocates ~300k cons cells against a
;; constant 200-cell live structure, exercising the GC mark/sweep
;; cycle repeatedly. A non-20100 result indicates the GC has
;; corrupted the top-level binding.

(print "=== Allocation churn ===\n")

(define list-size 200)
(define iters 1000)
(define expected-sum 20100)  ; sum 1..200

(define (build n acc)
  (if (= n 0) acc (build (- n 1) (cons n acc))))

(define data (build list-size '()))

(define (run i)
  (if (= i 0)
      'done
      (begin
        (map (lambda (x) (* x 2)) data)
        (filter (lambda (x) (= 0 (modulo x 2))) data)
        (run (- i 1)))))

(print "Running ") (print iters)
(print " iterations of (map + filter) over a ")
(print list-size) (print "-element list...\n")

(define t-start (time))
(run iters)
;; Use 2-arg reduce: 3-arg form mis-checks argv[1] as the list.
(define final-sum (reduce + data))
(define t-end (time))

(define elapsed (- t-end t-start))
;; map allocates list-size cons cells, filter allocates ~half that
;; (half of 1..200 is even). ~300 cells per iteration.
(define cells-per-iter (+ list-size (quotient list-size 2)))
(define total-allocations (* iters cells-per-iter))

(print "  data sum after churn: ") (print final-sum) (print "\n")
(print "  expected:             ") (print expected-sum) (print "\n")
(print "  elapsed:              ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:                 ~")
      (print (quotient (* total-allocations 1000) elapsed))
      (print " allocs/sec\n"))
    'skip)

(if (= final-sum expected-sum)
    (print "Status: PASS\n")
    (print "Status: FAIL (data list corrupted by GC, see KNOWN-ISSUES.md #2)\n"))
