;; Crash probe: right-nested cons spine vs. the GC mark phase.
;;
;; Builds a right-nested cons spine of `depth` cells, keeps it live, and
;; then allocates until a collection runs with the spine reachable. The
;; result is a binary signal: the VM either completes or aborts. Since
;; the marker walks cdr spines from a work list (core/vm-memory.c), one
;; slot however deep the spine is, any depth the heap can hold should
;; complete on any target. Raise `depth` and re-run after touching the
;; mark phase.
;;
;; Two conditions decide whether a run means anything:
;;
;;   - The spine has to be long enough to cross VM_GC_MIN_ALLOCATED
;;     (half the heap) while it is still live, or no sweep happens, the
;;     marker never runs, and the probe passes without testing anything.
;;     At the POSIX default heap that takes ~150k cells.
;;   - An optimizing compiler turns a recursive marker's cdr descent
;;     into a jump, which pushes the fault well past where it would
;;     otherwise appear. Against a recursive marker at -O3 under
;;     `ulimit -s 512`, the boundary sat between 150k and 200k cells;
;;     with the ~2 kB native stack of a Zoul it arrives three orders of
;;     magnitude sooner.
;;
;; On hosted targets, then, lower the stack limit (`ulimit -s 512`) and
;; keep the depth in the hundreds of thousands. On real hardware the
;; default depth below is already far past the edge.

(print "=== Crash-spine probe ===\n")

(define depth 200000)
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
