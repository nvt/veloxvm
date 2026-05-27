;; Fork-join mergesort demo for VeloxVM threading.
;;
;; Splits a deterministic pseudo-random list into N chunks, spawns one
;; worker thread per chunk to mergesort it, then joins each worker to
;; retrieve its sorted sublist and merges the results pairwise on the
;; main thread. The same input is also sorted sequentially; the two
;; outputs are compared element-by-element to verify the concurrent
;; path produced the right answer.
;;
;; This is a concurrency demo, not a parallelism demo. VeloxVM threads
;; multiplex onto a single OS thread (see doc/threading.md), so this
;; fork-join path does not beat the sequential one on wall-clock --
;; it actually pays a small thread setup + join overhead. The point
;; is to exercise thread-create! + thread-join!'s return-value
;; plumbing with a workload that self-validates: fork-join through
;; thread-join!'s return value, no shared mutable state, no mutex or
;; condition variable.

(define list-size 400)
(define num-workers 4)

;; Deterministic pseudo-random list generator (small LCG; constants
;; keep the multiply inside VeloxVM's 32-bit integer range).
(define (gen-list n seed)
  (let loop ((n n) (seed seed) (acc '()))
    (if (= n 0)
        (reverse acc)
        (let ((next (modulo (+ (* seed 1103) 12345) 7919)))
          (loop (- n 1) next (cons next acc))))))

(define (take lst n)
  (let loop ((lst lst) (n n) (acc '()))
    (if (or (= n 0) (null? lst))
        (reverse acc)
        (loop (cdr lst) (- n 1) (cons (car lst) acc)))))

(define (drop lst n)
  (if (or (= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

;; Tail-recursive merge of two sorted lists. The accumulator is built
;; in reverse and unwound at the end so the recursion stays in a
;; single frame regardless of list length -- VM_CONTEXT_STACK_SIZE is
;; only 64 frames on the POSIX port.
(define (merge a b)
  (let loop ((a a) (b b) (acc '()))
    (cond ((null? a) (append (reverse acc) b))
          ((null? b) (append (reverse acc) a))
          ((< (car a) (car b))
           (loop (cdr a) b (cons (car a) acc)))
          (else
           (loop a (cdr b) (cons (car b) acc))))))

;; Standard recursive mergesort. Recursion depth is O(log n).
(define (mergesort lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((half (quotient (length lst) 2)))
        (merge (mergesort (take lst half))
               (mergesort (drop lst half))))))

;; Split a list into K chunks. The first K-1 chunks have floor(n/k)
;; elements; the last chunk gets the remainder.
(define (split-into-chunks lst k)
  (let ((chunk-size (quotient (length lst) k)))
    (let loop ((lst lst) (i 0) (acc '()))
      (if (>= i (- k 1))
          (reverse (cons lst acc))
          (loop (drop lst chunk-size)
                (+ i 1)
                (cons (take lst chunk-size) acc))))))

;; Spawn one worker per chunk; each worker mergesorts its chunk and
;; returns the sorted list as its thunk result, which thread-join!
;; surfaces to the main thread.
(define (spawn-workers chunks)
  (let loop ((chunks chunks) (acc '()))
    (if (null? chunks)
        (reverse acc)
        (let ((chunk (car chunks)))
          (loop (cdr chunks)
                (cons (thread-create! (lambda () (mergesort chunk)))
                      acc))))))

;; thread-join! each worker in order; the call blocks until the
;; worker finishes and returns its sorted sublist.
(define (join-workers workers)
  (let loop ((workers workers) (acc '()))
    (if (null? workers)
        (reverse acc)
        (loop (cdr workers)
              (cons (thread-join! (car workers)) acc)))))

;; Merge K sorted lists into one. Linear-recursive fold (depth = K),
;; which is fine for the small K used here.
(define (merge-all sorted-chunks)
  (if (null? (cdr sorted-chunks))
      (car sorted-chunks)
      (merge (car sorted-chunks)
             (merge-all (cdr sorted-chunks)))))

;; Element-by-element comparison. equal? would also work, but a
;; manual walk avoids any dependency on equal?'s list handling.
(define (lists-equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((= (car a) (car b)) (lists-equal? (cdr a) (cdr b)))
        (else #f)))

(print "Parallel mergesort demo\n")
(print "  list size:   ") (print list-size) (print "\n")
(print "  workers:     ") (print num-workers) (print "\n")

(define input (gen-list list-size 42))

(print "Sequential sort... ")
(define t0 (time))
(define sequential-result (mergesort input))
(define t1 (time))
(print (- t1 t0)) (print " ms\n")

(print "Parallel sort... ")
(define t2 (time))
(define chunks (split-into-chunks input num-workers))
(define workers (spawn-workers chunks))
(define sorted-chunks (join-workers workers))
(define parallel-result (merge-all sorted-chunks))
(define t3 (time))
(print (- t3 t2)) (print " ms\n")

(if (lists-equal? sequential-result parallel-result)
    (print "PASS: parallel and sequential results match\n")
    (print "FAIL: parallel and sequential results differ\n"))
