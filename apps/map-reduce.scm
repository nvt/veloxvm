(define x (list 500 400 300))
(define x2 (list 500))
(define y (list 0 "Hej" -92 8/5 (list 1 2 3)))

(reduce / x)
(reduce / x2)
(define number-test (map number? y))
(print "Which elements of " y " are numbers? " number-test #\Newline)
(thread-sleep! 1000)
