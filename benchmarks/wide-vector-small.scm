;; Wide-fanout vector, scaled for small-heap configs (§7.3).
;; Sized to fit a Zoul-tier pool (125 slots): 30-element vector with
;; 2-cell sublists = ~90 cells live, comfortably below the gate.

(print "=== Wide-fanout vector (small) ===\n")

(define width 30)
(define gc-cycles 3000)
(define throwaway-size 30)

(define wide (make-vector width '()))

(define (fill-iter i)
  (if (= i width)
      'done
      (begin
        (vector-set! wide i (cons i (cons (* i 2) '())))
        (fill-iter (+ i 1)))))

(fill-iter 0)
(print "Vector: ") (print width) (print " 2-cell entries\n")

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

(define first-el (vector-ref wide 0))
(define last-el (vector-ref wide (- width 1)))
(define ok (and (= (car first-el) 0)
                (= (car last-el) (- width 1))))

(print "  intact:  ") (print (if ok "yes" "NO")) (print "\n")
(print "  elapsed: ") (print elapsed) (print " ms\n")

(if ok (print "Status: PASS\n") (print "Status: FAIL\n"))
