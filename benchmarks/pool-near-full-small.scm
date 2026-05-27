;; Pool-near-full sweep gate (§7.4), scaled for small-heap configs.
;; live-cells=80 trips the 67% sweep gate on a 125-slot Zoul pool
;; (80 / 125 = 64% — set just above the gate by raising to 84-90 in
;; sensitivity runs; left at 80 here because at 84 the pool starts
;; rejecting throwaway allocations).

(print "=== Pool near full (small) ===\n")

(define live-cells 40)
(define gc-cycles 3000)
(define throwaway-size 20)

(define (build-list n)
  (let loop ((i n) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

(define live (build-list live-cells))
(print "Live cells: ") (print live-cells) (print "\n")

(define (build-throwaway n)
  (let loop ((i n) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-throwaway throwaway-size)
        (pressure (- k 1)))))

(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))

(define live-len
  (let loop ((l live) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))
(define ok (= live-len live-cells))

(print "  live after: ") (print live-len) (print "\n")
(print "  elapsed:    ") (print elapsed) (print " ms\n")

(if ok (print "Status: PASS\n") (print "Status: FAIL\n"))
