;; Symbol-table × thread count (§7.2), small-tier.
;; 8 small top-level bindings + 2 worker threads + main = 3 active.

(print "=== Shared symtab × threads (small) ===\n")

(define v1 (make-vector 4 1))
(define v2 (make-vector 4 1))
(define v3 (make-vector 4 1))
(define v4 (make-vector 4 1))
(define v5 (make-vector 4 1))
(define v6 (make-vector 4 1))
(define v7 (make-vector 4 1))
(define v8 (make-vector 4 1))

(define worker-count 2)
(define iters-per-worker 1500)
(define throwaway-size 25)

(define (build-throwaway n)
  (let loop ((i n) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

(define (worker n)
  (if (= n 0) 'done
      (begin
        (build-throwaway throwaway-size)
        (worker (- n 1)))))

(define (spawn n)
  (if (= n 0) 'done
      (begin
        (thread-create! (lambda () (worker iters-per-worker)))
        (spawn (- n 1)))))

(spawn worker-count)
(print worker-count) (print " worker(s) spawned\n")

(define main-cycles 3000)
(define t-start (time))
(worker main-cycles)
(define t-end (time))

(print "  v1 still vector: ") (print (vector? v1)) (print "\n")
(print "  elapsed:         ") (print (- t-end t-start)) (print " ms\n")

(if (and (vector? v1) (vector? v8))
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
