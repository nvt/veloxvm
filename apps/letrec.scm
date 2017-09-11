(letrec ((even?
	  (lambda (n)
	    (if (zero? n)
		#t
		(odd? (- n 1)))))
	 (odd?
	  (lambda (n)
	    (if (zero? n)
		#f
		(even? (- n 1))))))
  (print "Is 10 even? " (even? 10) #\Newline)
  (print "Is 89135 even? " (even? 89135) #\Newline)
  (print "Is 3316589134 even? " (even? 3316589134) #\Newline))
