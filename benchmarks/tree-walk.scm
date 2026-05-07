;; Binary tree build + sum, repeated.
;;
;; Builds a fully-balanced binary tree to depth D, sums its leaves,
;; and repeats. Each node is a proper 2-element list (left right),
;; so a depth-D tree uses 2 * (2^D - 1) cons cells.
;;
;; Why proper lists, not (cons left right): the VM has a bug in cdr
;; on improper lists of length >= 2 -- (cdr (cons 1 (cons 2 3))) errors
;; with "Internal error". A natural cons-pair encoding hits this on
;; every internal node. Using (list left right) avoids it.
;;
;; Stack budget. With VM_CONTEXT_STACK_SIZE = 64 and ~7 frames per
;; recursion level for sum-tree (if + bind + lambda + + + car + cdr),
;; the depth cap is around D = 9. We repeat the build/sum cycle to
;; get meaningful timing and to keep the GC busy.
;;
;; What this exercises (different from existing benchmarks):
;;   * Branched live structure during walk (deep-structures.scm uses
;;     a single long spine)
;;   * Allocation-then-discard cycle for whole structures
;;   * Iterative GC mark on a binary tree shape

(print "=== Tree build + walk (repeated) ===\n")

(define depth 9)
(define repeats 500)
(define expected-sum 512)  ; 2^9 leaves, each leaf = 1
(define cells-per-tree 1022)  ; 2 * (2^9 - 1)

(define (make-tree d)
  (if (= d 0)
      1
      (list (make-tree (- d 1)) (make-tree (- d 1)))))

(define (sum-tree t)
  (if (pair? t)
      (+ (sum-tree (car t)) (sum-tree (car (cdr t))))
      t))

(define (cycle k last-sum)
  (if (= k 0)
      last-sum
      (cycle (- k 1) (sum-tree (make-tree depth)))))

(print "Building+summing a depth-") (print depth)
(print " tree x ") (print repeats) (print " times\n")
(print "(") (print (* repeats cells-per-tree)) (print " total cons allocations)\n")

(define t-start (time))
(define final-sum (cycle repeats 0))
(define t-end (time))

(define elapsed (- t-end t-start))
(define total-allocations (* repeats cells-per-tree))

(print "  final leaf sum: ") (print final-sum) (print "\n")
(print "  expected:       ") (print expected-sum) (print "\n")
(print "  elapsed:        ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:           ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if (= final-sum expected-sum)
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
