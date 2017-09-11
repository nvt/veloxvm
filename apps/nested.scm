(define result
	(number? (+ 1 (+ 1 1) (* 3 1)
		    (* 4 
		       (/ 50 10)
		       (- 12 (+ 4 2)))
            	       7)))

(print "Was it a number? " result #\Newline)
