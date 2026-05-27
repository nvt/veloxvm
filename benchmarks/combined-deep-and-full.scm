;; Compounded mark + sweep stress.
;;
;; Combines three shapes that individually live in deep-structures.scm
;; (depth), memory-stress.scm (multiple roots), and pool-near-full.scm
;; (sweep gate). Each GC cycle pays:
;;   - Recursive mark on five separate spine roots
;;   - Sweep over a pool kept above the 2/3 gate
;; This is the "max stress" baseline for current-marker pause time.
;; After linearization, the recursive depth cost should disappear and
;; only the multi-root + sweep cost remain.
;;
;; spine-depth is set well below the ~40-frame Zoul crash point to
;; remain portable; this benchmark measures slowdown, not crashes
;; (see crash-spine.scm for the crash probe).

(print "=== Combined: deep × multi-root × full pool ===\n")

(define spine-depth 30)
(define gc-cycles 3000)
(define throwaway-size 300)

(define (build-deep d)
  (define (iter d acc)
    (if (= d 0) acc (iter (- d 1) (cons d acc))))
  (iter d '()))

(define spine-a (build-deep spine-depth))
(define spine-b (build-deep spine-depth))
(define spine-c (build-deep spine-depth))
(define spine-d (build-deep spine-depth))
(define spine-e (build-deep spine-depth))

;; Additional padding to push the pool above the 2/3 sweep gate on
;; small-pool targets (Zoul). Harmless on hosts where the pool is
;; far larger than this; the cost shows only when the gate trips.
(define padding (build-deep 60))

(print "Five spines of depth ") (print spine-depth)
(print " + ") (print 60) (print "-cell padding\n")

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-deep throwaway-size)
        (pressure (- k 1)))))

(print "Forcing GC pressure with ") (print gc-cycles)
(print " throwaway spines of ") (print throwaway-size) (print " cells...\n")

(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))
(define total-allocations (* gc-cycles throwaway-size))

(define (depth-of lst)
  (let loop ((l lst) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))

(define ok
  (and (= (depth-of spine-a) spine-depth)
       (= (depth-of spine-b) spine-depth)
       (= (depth-of spine-c) spine-depth)
       (= (depth-of spine-d) spine-depth)
       (= (depth-of spine-e) spine-depth)))

(print "  all five spines intact: ") (print (if ok "yes" "NO")) (print "\n")
(print "  elapsed:                ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:                   ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if ok
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
