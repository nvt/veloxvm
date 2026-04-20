;; Test GC with deep structures to verify iterative marking works
;; Previous recursive GC implementation would overflow C stack at depth ~40
;; Uses tail-recursive list construction to avoid VM stack overflow

(define (build-deep-list depth value)
  (define (build-iter d acc)
    (if (= d 0)
        acc
        (build-iter (- d 1) (cons value acc))))
  (build-iter depth '()))

;; Tail-recursive list depth counter
(define (list-depth lst)
  (define (depth-iter l count)
    (if (null? l)
        count
        (depth-iter (cdr l) (+ count 1))))
  (depth-iter lst 0))

(print "=== Testing GC with Deep Structures ===\n")

;; Test 1: Depth 60 (would overflow with recursive marking)
(print "\nTest 1: Creating list of depth 60...\n")
(define deep60 (build-deep-list 60 "deep60"))
(print "Created list, verifying depth = ")
(print (list-depth deep60))
(print "\n")
(if (= (list-depth deep60) 60)
    (print "  PASS: Depth 60 constructed successfully\n")
    (print "  FAIL: Depth incorrect\n"))

;; Test 2: Depth 100
(print "\nTest 2: Creating list of depth 100...\n")
(define deep100 (build-deep-list 100 "deep100"))
(print "Created list, verifying depth = ")
(print (list-depth deep100))
(print "\n")
(if (= (list-depth deep100) 100)
    (print "  PASS: Depth 100 constructed successfully\n")
    (print "  FAIL: Depth incorrect\n"))

;; Test 3: Depth 200 (well beyond old recursive limit)
(print "\nTest 3: Creating list of depth 200...\n")
(define deep200 (build-deep-list 200 "deep200"))
(print "Created list, verifying depth = ")
(print (list-depth deep200))
(print "\n")
(if (= (list-depth deep200) 200)
    (print "  PASS: Depth 200 constructed successfully\n")
    (print "  FAIL: Depth incorrect\n"))

;; Create some memory pressure to trigger GC automatically
(print "\nCreating additional structures to trigger GC...\n")
(define extra1 (build-deep-list 150 "extra"))
(define extra2 (build-deep-list 150 "extra"))
(define extra3 (build-deep-list 150 "extra"))

;; Verify original deep structures still intact
(print "\nVerifying deep structures survive automatic GC:\n")
(if (and (= (list-depth deep60) 60)
         (= (list-depth deep100) 100)
         (= (list-depth deep200) 200))
    (print "  PASS: All deep structures survived GC\n")
    (print "  FAIL: Some structures corrupted\n"))

(print "\n=== All deep structure tests completed ===\n")
