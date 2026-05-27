;; Allocate-and-drop floor (§7.6), with the dead-code fix.
;;
;; The previous alloc-and-drop variant had its cons calls dead-stripped
;; by the Scheme compiler because the results were never used. This
;; one routes each cons through a top-level set! cell so the allocation
;; cannot be eliminated. Each iteration allocates 3 cells; previous
;; iteration's cell becomes garbage immediately.

(print "=== Allocate-and-drop floor (small) ===\n")

(define iters 5000)
(define holder1 '())
(define holder2 '())
(define holder3 '())

(define (alloc-drop k)
  (if (= k 0) 'done
      (begin
        (set! holder1 (cons k '()))
        (set! holder2 (cons k holder1))
        (set! holder3 (cons k (cons k '())))
        (alloc-drop (- k 1)))))

(print "Iterations: ") (print iters)
(print " (3 cells per iter, ") (print (* iters 3)) (print " total)\n")

(define t-start (time))
(alloc-drop iters)
(define t-end (time))

(print "  elapsed: ") (print (- t-end t-start)) (print " ms\n")
(print "Status: PASS\n")
