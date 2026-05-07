;; Sieve of Eratosthenes up to N, backed by a vector.
;;
;; Stresses three things that no other benchmark touches together:
;;   * vector-ref / vector-set! at scale (N+1 element vector)
;;   * integer arithmetic in tight loops (multiplication, comparison)
;;   * nested tail recursion (mark-multiples inside mark-composites)
;;
;; Vector ops go through a separate code path from cons cells
;; (vm-objects.h vm_vector_t with VM_MALLOC'd buffer), so this is
;; complementary to the cons-heavy benchmarks.

(print "=== Sieve of Eratosthenes ===\n")

(define n 10000)
(define expected-count 1229)

(define sieve (make-vector (+ n 1) #t))
(vector-set! sieve 0 #f)
(vector-set! sieve 1 #f)

(define (mark-multiples j step)
  (if (> j n)
      'done
      (begin
        (vector-set! sieve j #f)
        (mark-multiples (+ j step) step))))

(define (mark-composites i)
  (if (> (* i i) n)
      'done
      (begin
        (if (vector-ref sieve i)
            (mark-multiples (* i i) i)
            'skip)
        (mark-composites (+ i 1)))))

(define (count-primes i acc)
  (if (> i n)
      acc
      (count-primes (+ i 1)
                    (if (vector-ref sieve i) (+ acc 1) acc))))

(print "Sieving up to ") (print n) (print "...\n")
(define t-start (time))
(mark-composites 2)
(define count (count-primes 0 0))
(define t-end (time))

(define elapsed (- t-end t-start))

(print "  primes found: ") (print count) (print "\n")
(print "  expected:     ") (print expected-count) (print "\n")
(print "  elapsed:      ") (print elapsed) (print " ms\n")
(if (> elapsed 0)
    (begin
      (print "  rate:         ")
      (print (quotient (* n 1000) elapsed))
      (print " cells-scanned/sec\n"))
    'skip)

(if (= count expected-count)
    (print "Status: PASS\n")
    (print "Status: FAIL\n"))
