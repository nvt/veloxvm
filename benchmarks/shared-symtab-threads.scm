;; Symbol-table mark cost × thread count.
;;
;; mark_thread_references walks thread->program->symbols for every
;; thread (vm-memory.c:263-265), with no memory_is_marked guard on
;; the symbol-bindings loop. Threads sharing a program re-walk the
;; same bindings every GC. This benchmark binds many top-level
;; vectors, then spawns several allocator threads -- every GC pays
;; (thread-count + 1) full symbol-table mark walks.
;;
;; For differential measurement, run twice: thread-count = 0 (just
;; main) and thread-count = N. The runtime delta divided by GC count
;; reveals the per-thread symbol-walk overhead. After the
;; one-line memory_is_marked guard fix, the delta should collapse to
;; near zero (each binding marked once across all threads).

(print "=== Shared symbol-table × threads ===\n")

;; Twenty top-level vectors. Each becomes a symbol_bindings entry
;; that the marker re-enters once per thread on every GC.
(define v01 (make-vector 30 1))
(define v02 (make-vector 30 1))
(define v03 (make-vector 30 1))
(define v04 (make-vector 30 1))
(define v05 (make-vector 30 1))
(define v06 (make-vector 30 1))
(define v07 (make-vector 30 1))
(define v08 (make-vector 30 1))
(define v09 (make-vector 30 1))
(define v10 (make-vector 30 1))
(define v11 (make-vector 30 1))
(define v12 (make-vector 30 1))
(define v13 (make-vector 30 1))
(define v14 (make-vector 30 1))
(define v15 (make-vector 30 1))
(define v16 (make-vector 30 1))
(define v17 (make-vector 30 1))
(define v18 (make-vector 30 1))
(define v19 (make-vector 30 1))
(define v20 (make-vector 30 1))

(define worker-count 4)
(define iters-per-worker 5000)
(define throwaway-size 100)

(define (build-throwaway n acc)
  (if (= n 0) acc (build-throwaway (- n 1) (cons n acc))))

(define (worker n)
  (if (= n 0) 'done
      (begin
        (build-throwaway throwaway-size '())
        (worker (- n 1)))))

(define (spawn n)
  (if (= n 0) 'done
      (begin
        (thread-create! (lambda () (worker iters-per-worker)))
        (spawn (- n 1)))))

(print "Spawning ") (print worker-count) (print " worker threads...\n")
(spawn worker-count)

(define main-cycles 8000)
(print "Main thread runs ") (print main-cycles)
(print " allocation cycles alongside workers...\n")

(define t-start (time))
(worker main-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))
(define total-allocations
  (* (+ main-cycles (* worker-count iters-per-worker)) throwaway-size))

(print "  20 top-level bindings still vectors: ")
(print (if (and (vector? v01) (vector? v20)) "yes" "NO")) (print "\n")
(print "  elapsed (main):                      ")
(print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate (across all threads):           ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if (and (vector? v01) (vector? v20))
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
