(define v (make-vector 5))

(define f1 (lambda (i)
	     (vector-set! v i (* i i))))

(for-each f1 (list 0 1 2 3 4))

(print "v = " v #\Newline)
