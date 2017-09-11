(display (guard (obj
	((eq? obj 'Div0Exception)
	 (display "Caught a div0 exception")
	 (newline)
         #f)
	(else
	 (display "Caught an unknown exception: ")
	 (write obj)
	 (newline)))
       (display "Testing exceptions")
       (newline)
       (/ 5 0) ; This expression should generate a Div0Exception.
       (display "It did not work if we got here")
       (newline)
       #t))
(newline)
