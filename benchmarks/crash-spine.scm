;; Crash probe: right-nested cons spine vs recursive marker.
;;
;; Builds a right-nested cons spine of `depth` cells, keeps it live,
;; then runs allocation pressure to force GC. mark_object recurses
;; once per cell of the spine (vm-memory.c:170-178). On targets with
;; small native C stacks (Zoul: ~2 kB), the recursion overflows
;; somewhere around depth 40. On POSIX (8 MB native stack), any
;; practical depth survives -- run this on real hardware to see the
;; effect.
;;
;; Use as a binary signal: did the VM complete or abort? Sweep
;; `depth` upward across runs (60, 80, 100, 200, 500, ...) until the
;; crash point is found.
;;
;; After linearization, all values of `depth` up to the live mark-
;; bearing object count should complete successfully on every target.
;; This file then becomes a regression test rather than a probe.

(print "=== Crash-spine probe ===\n")

(define depth 200)
(define pressure-iters 200)
(define throwaway-size 100)

(define (build-deep d)
  (define (iter d acc)
    (if (= d 0) acc (iter (- d 1) (cons d acc))))
  (iter d '()))

(define spine (build-deep depth))
(print "Built spine of depth ") (print depth) (print "\n")

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-deep throwaway-size)
        (pressure (- k 1)))))

(print "Running ") (print pressure-iters)
(print " pressure iterations to force GC...\n")

(define t-start (time))
(pressure pressure-iters)
(define t-end (time))

(define final-depth
  (let loop ((l spine) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))

(print "  spine depth after pressure: ") (print final-depth) (print "\n")
(print "  expected:                   ") (print depth) (print "\n")
(print "  elapsed:                    ") (print (- t-end t-start))
(print " ms\n")

(if (= final-depth depth)
    (begin
      (print "Status: PASS (no crash at depth ") (print depth) (print ")\n"))
    (print "Status: FAIL (spine damaged)\n"))
