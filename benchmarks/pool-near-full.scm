;; Object-pool near-full sweep cost.
;;
;; The pool sweep is gated to fire only when items >= 2 * capacity / 3
;; (vm-mempool.c:290). Below that threshold the ref-bitmap is reset
;; but no slot is reclaimed. This benchmark pre-allocates enough live
;; cons cells to push the pool above the gate, then runs sustained
;; allocation pressure. Every subsequent GC pays the full bitmap-scan
;; sweep cost on top of the mark walk.
;;
;; Target relevance: most useful on Zoul (pool capacity = 3000 B /
;; ~24 B = 125 slots, so ~85 live cells trips the gate). On POSIX the
;; default pool is huge (~400k slots) and this benchmark will not trip
;; the gate at the configured `live-cells` count -- adjust upward to
;; reach 67% of the actual pool capacity if measuring there.

(print "=== Pool near full ===\n")

(define live-cells 100)
(define gc-cycles 5000)
(define throwaway-size 200)

(define (build-list n acc)
  (if (= n 0) acc (build-list (- n 1) (cons n acc))))

(define live (build-list live-cells '()))
(print "Live structure: ") (print live-cells) (print " cons cells\n")

(define (build-throwaway n acc)
  (if (= n 0) acc (build-throwaway (- n 1) (cons n acc))))

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-throwaway throwaway-size '())
        (pressure (- k 1)))))

(print "Forcing GC pressure with ") (print gc-cycles)
(print " throwaway lists of ") (print throwaway-size) (print " cells...\n")

(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))
(define total-allocations (* gc-cycles throwaway-size))

(define live-len
  (let loop ((l live) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))
(define ok (= live-len live-cells))

(print "  live cells after pressure: ") (print live-len) (print "\n")
(print "  elapsed:                   ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:                      ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if ok
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
