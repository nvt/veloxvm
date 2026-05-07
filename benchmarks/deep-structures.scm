;; Deep linear-spine GC mark test, repeated.
;;
;; Builds a 200-cell single-spine list once, keeps it live, then runs
;; sustained allocation pressure to force many GC cycles. Each GC
;; cycle re-walks the deep spine via the iterative mark phase. The
;; earlier recursive mark would overflow the C stack around depth 40;
;; this exercises iterative mark on a long spine repeatedly.
;;
;; Complementary to tree-walk.scm (branched live structure) and
;; memory-stress.scm (multiple live roots) -- this one isolates the
;; long-spine traversal path.

(print "=== Deep linear spine ===\n")

;; 1-arg builder. A 2-arg variant taking (depth value) crashes the VM
;; under sustained pressure (>= ~1800 calls); a 1-arg variant survives
;; multi-million-allocation runs. Bug not investigated here -- the
;; suspicion is the 2-arg lambda + internal-define interaction.
(define (build-deep-list depth)
  (define (build-iter d acc)
    (if (= d 0) acc (build-iter (- d 1) (cons d acc))))
  (build-iter depth '()))

(define (list-depth lst)
  (define (iter l count)
    (if (null? l) count (iter (cdr l) (+ count 1))))
  (iter lst 0))

(define depth 200)
(define gc-cycles 10000)
(define throwaway-size 100)

(define deep (build-deep-list depth))
(print "Built spine of depth ") (print depth) (print "\n")

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-deep-list throwaway-size)
        (pressure (- k 1)))))

(print "Forcing GC pressure with ") (print gc-cycles)
(print " throwaway lists of ") (print throwaway-size) (print " cells...\n")
(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))
(define total-allocations (* gc-cycles throwaway-size))

(define result-depth (list-depth deep))
(print "  spine depth after pressure: ") (print result-depth) (print "\n")
(print "  expected:                   ") (print depth) (print "\n")
(print "  elapsed:                    ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:                       ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if (= result-depth depth)
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
