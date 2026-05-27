;; Many live ext-objects (§7.5), scaled for small-heap configs.
;; 20 mutexes is enough to make the finalize-list walk visible if
;; it matters; small enough to fit comfortably.

(print "=== Ext-objects finalize-walk (small) ===\n")

(define mutex-count 20)
(define gc-cycles 3000)
(define throwaway-size 30)

(define (make-mutexes n)
  (let loop ((i n) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons (make-mutex "m") acc)))))

(define mutexes (make-mutexes mutex-count))
(print "Live mutexes: ") (print mutex-count) (print "\n")

(define (build-throwaway n)
  (let loop ((i n) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

(define (pressure k)
  (if (= k 0) 'done
      (begin
        (build-throwaway throwaway-size)
        (pressure (- k 1)))))

(define t-start (time))
(pressure gc-cycles)
(define t-end (time))
(define elapsed (- t-end t-start))

(define live-len
  (let loop ((l mutexes) (n 0))
    (if (null? l) n (loop (cdr l) (+ n 1)))))
(define ok (= live-len mutex-count))

(print "  live after: ") (print live-len) (print "\n")
(print "  elapsed:    ") (print elapsed) (print " ms\n")

(if ok (print "Status: PASS\n") (print "Status: FAIL (mutex list damaged)\n"))
