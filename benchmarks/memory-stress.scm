;; Multi-root GC mark test.
;;
;; Builds 10 separate lists of 200 cells each, all kept live as
;; top-level bindings simultaneously (2000 cells across 10 distinct
;; roots), then runs sustained allocation pressure to force many GC
;; cycles. Each GC cycle must mark all 10 roots.
;;
;; Distinct from tree-walk.scm (one live tree) and alloc-churn.scm
;; (one live data list) -- the value here is exercising the GC's
;; root-set traversal across multiple top-level bindings under load.

(print "=== Multi-root GC mark ===\n")

(define (build-list-iter n)
  (define (iter i acc)
    (if (> i n) acc (iter (+ i 1) (cons i acc))))
  (iter 1 '()))

(print "Building 10 lists of 200 elements each (2000 cells total)...\n")
(define list-a (build-list-iter 200))
(define list-b (build-list-iter 200))
(define list-c (build-list-iter 200))
(define list-d (build-list-iter 200))
(define list-e (build-list-iter 200))
(define list-f (build-list-iter 200))
(define list-g (build-list-iter 200))
(define list-h (build-list-iter 200))
(define list-i (build-list-iter 200))
(define list-j (build-list-iter 200))

(define gc-cycles 5000)
(define throwaway-size 500)

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-list-iter throwaway-size)
        (pressure (- k 1)))))

(print "Forcing GC pressure with ") (print gc-cycles)
(print " throwaway lists of ") (print throwaway-size) (print " cells...\n")
(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))
(define total-allocations (* gc-cycles throwaway-size))

(define ok
  (and (= (car list-a) 200) (= (car list-b) 200)
       (= (car list-c) 200) (= (car list-d) 200)
       (= (car list-e) 200) (= (car list-f) 200)
       (= (car list-g) 200) (= (car list-h) 200)
       (= (car list-i) 200) (= (car list-j) 200)))

(print "  all 10 root lists intact: ") (print (if ok "yes" "NO")) (print "\n")
(print "  elapsed:                  ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:                     ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if ok
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
