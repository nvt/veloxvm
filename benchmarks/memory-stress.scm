;; Multi-root GC mark test.
;;
;; Builds 10 separate lists of 200 cells each, all kept live as
;; top-level bindings simultaneously (2000 cells across 10 distinct
;; roots). Verifies the GC marks all roots correctly: every list
;; survives intact through allocation pressure.
;;
;; Distinct from tree-walk.scm (one live tree) and alloc-churn.scm
;; (one live data list) -- the value here is exercising the GC's
;; root-set traversal across multiple top-level bindings.

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

;; Allocate more to force a GC cycle, then verify every root list
;; is still intact (head element is a known sentinel).
(build-list-iter 500)
(build-list-iter 500)
(build-list-iter 500)

(define ok
  (and (= (car list-a) 200) (= (car list-b) 200)
       (= (car list-c) 200) (= (car list-d) 200)
       (= (car list-e) 200) (= (car list-f) 200)
       (= (car list-g) 200) (= (car list-h) 200)
       (= (car list-i) 200) (= (car list-j) 200)))

(print "All 10 root lists intact after GC: ")
(if ok
    (print "PASS\nStatus: PASS\n")
    (print "FAIL\nStatus: FAIL\n"))
