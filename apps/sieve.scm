;; An implementation of Eratosthenes's Sieve.

(define primes-ready #f)

(define primes (make-vector 10000 #t))

(define sieve-mark-multiples
  (lambda (start-num num limit)
    (let ((next-num (+ start-num num)))
      (when (<= next-num limit)
	    (vector-set! primes next-num #f)
	    (sieve-mark-multiples start-num next-num limit)))))

(define find-next-unmarked
  (lambda (num limit)
    (if (> num limit)
	#f
	(begin
	  (if (vector-ref primes num)
	      num
	      (find-next-unmarked (+ num 1) limit))))))

(define sieve-tail
  (lambda (num limit)
    (sieve-mark-multiples num num limit)
    (let ((next-num (find-next-unmarked (+ num 1) limit)))
      (if next-num
	  (sieve-tail next-num limit)
	  #t))))

(define sieve
  (lambda (limit)
    (sieve-tail 2 limit)))

(define generate-primes
  (lambda (limit)
    (vector-set! primes 0 #f)
    (vector-set! primes 1 #f)
    (sieve limit)
    (set! primes-ready #t)))

(define prime?
  (lambda (num)
    (let ((limit (- (vector-length primes) 1)))
      (unless primes-ready
	      (print "Finding all primes up to " limit "...")
	      (generate-primes limit)
	      (print "done!" #\Newline))
      (cond
       ((> num limit)
	"too large number")
       ((< num 2)
	#f)
       (else
	(vector-ref primes num))))))

(define test-numbers (list -5 0 1 2 11 50 83 1899 3931 70001))
(print "Which of the numbers " test-numbers " are primes? "
       (map prime? test-numbers) #\Newline)

(define print-if-prime
  (lambda (current prime-vector)
    (when (< current (vector-length prime-vector))
	  (if (vector-ref prime-vector current)
	      (print current " is a prime" #\Newline))
	  (print-if-prime (+ current 1) prime-vector))))

(print-if-prime 0 primes)

(let ((prime-count (count
		    (lambda (x) (equal? x #t))
		    (vector->list primes))))
  (print "Number of primes: " prime-count #\Newline))
