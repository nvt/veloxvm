;; Many live ext-objects: finalize-list walk cost.
;;
;; Each (make-mutex) allocates a vm_ext_object_t and links it onto
;; ext_object_list_head (vm-memory.c:631). On every GC,
;; finalize_unmarked_ext_objects (vm-memory.c:447-467) walks the
;; whole list before the heap sweep, regardless of how many entries
;; are unmarked. This benchmark builds a long list of mutexes,
;; keeps them all live, then runs allocation pressure -- every GC
;; pays the O(mutex-count) finalize-list traversal.
;;
;; Compared against alloc-churn.scm (no ext-objects), the runtime
;; delta isolates the ext-object finalize-walk cost per GC cycle.

(print "=== Ext-objects finalize-walk ===\n")

(define mutex-count 100)
(define gc-cycles 5000)
(define throwaway-size 200)

(define (make-mutexes n acc)
  (if (= n 0) acc
      (make-mutexes (- n 1) (cons (make-mutex "m") acc))))

(define mutexes (make-mutexes mutex-count '()))
(print "Live mutexes: ") (print mutex-count) (print "\n")

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

(define live-len
  (let loop ((l mutexes) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))
(define ok (= live-len mutex-count))

(print "  live mutexes after pressure: ") (print live-len) (print "\n")
(print "  elapsed:                     ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:                        ")
      (print (* (quotient total-allocations elapsed) 1000))
      (print " allocs/sec\n"))
    'skip)

(if ok
    (print "Status: PASS\n")
    (print "Status: FAIL (mutex list damaged)\n"))
