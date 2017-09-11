(print "Result: "
       (let ((x-func
	     (lambda (x)
	       (if (> x 0) "pos" "neg"))))
	 (x-func 5)) #\Newline)
