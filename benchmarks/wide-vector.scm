;; Wide-fanout vector GC mark test.
;;
;; Builds one vector with N compound elements (a small 2-cell list
;; each), kept live as a top-level binding, then runs sustained
;; allocation pressure to force many GC cycles. Each cycle marks the
;; vector header, the elements array, and recurses into every
;; element. This isolates the VECTOR case of mark_object
;; (vm-memory.c:199-211) and its per-element recursion.
;;
;; Companion to deep-structures.scm (depth) and memory-stress.scm
;; (multiple roots). The expected scaling is linear in `width`: a
;; doubling of width should roughly double per-GC mark time.

(print "=== Wide-fanout vector ===\n")

(define width 500)
(define gc-cycles 5000)
(define throwaway-size 200)

(define wide (make-vector width '()))

(define (fill-iter i)
  (if (= i width)
      'done
      (begin
        (vector-set! wide i (cons i (cons (* i 2) '())))
        (fill-iter (+ i 1)))))

(fill-iter 0)
(print "Vector populated with ") (print width)
(print " 2-cell entries\n")

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

(define first-el (vector-ref wide 0))
(define last-el (vector-ref wide (- width 1)))
(define ok (and (= (car first-el) 0)
                (= (car last-el) (- width 1))))

(print "  vector intact: ") (print (if ok "yes" "NO")) (print "\n")
(print "  elapsed:       ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:          ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if ok
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
