;; Combined deep × multi-root × full pool, small-tier (§7.7).
;; 3 spines of 15 + 30-cell padding = ~75 live cells, fits Zoul pool.

(print "=== Combined (small) ===\n")

(define spine-depth 15)
(define gc-cycles 3000)
(define throwaway-size 30)

(define (build-deep d)
  (let loop ((i d) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

(define spine-a (build-deep spine-depth))
(define spine-b (build-deep spine-depth))
(define spine-c (build-deep spine-depth))
(define padding (build-deep 30))

(print "3 spines depth ") (print spine-depth)
(print " + 30 padding\n")

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-deep throwaway-size)
        (pressure (- k 1)))))

(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))

(define (depth-of lst)
  (let loop ((l lst) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))

(define ok
  (and (= (depth-of spine-a) spine-depth)
       (= (depth-of spine-b) spine-depth)
       (= (depth-of spine-c) spine-depth)))

(print "  intact:  ") (print (if ok "yes" "NO")) (print "\n")
(print "  elapsed: ") (print elapsed) (print " ms\n")

(if ok (print "Status: PASS\n") (print "Status: FAIL\n"))
