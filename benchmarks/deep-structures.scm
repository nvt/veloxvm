;; Deep linear-spine GC mark test.
;;
;; Builds a 200-cell list (single linear spine, no branching) and
;; verifies it survives a forced GC cycle. The earlier recursive
;; mark implementation overflowed the C stack around depth 40;
;; depth 200 proves the iterative mark walks arbitrary spines.
;;
;; Complementary to tree-walk.scm (branched live structure) and
;; memory-stress.scm (multiple live roots) -- this one isolates
;; the long-spine traversal path.

(print "=== Deep linear spine ===\n")

(define (build-deep-list depth value)
  (define (build-iter d acc)
    (if (= d 0) acc (build-iter (- d 1) (cons value acc))))
  (build-iter depth '()))

(define (list-depth lst)
  (define (iter l count)
    (if (null? l) count (iter (cdr l) (+ count 1))))
  (iter lst 0))

(define depth 200)
(define deep (build-deep-list depth "deep"))
(print "Built list of depth ") (print depth) (print "\n")

;; Allocate more to force a GC cycle.
(build-deep-list 150 "extra")
(build-deep-list 150 "extra")
(build-deep-list 150 "extra")

(define result-depth (list-depth deep))
(print "Depth after GC: ") (print result-depth) (print "\n")

(if (= result-depth depth)
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
