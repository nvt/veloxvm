(define fact-tail (lambda (result x)
  (if (<= x 1)
    result
    (fact-tail (* result x) (- x 1)))))

(define fact (lambda (n)
  (and (number? n) (fact-tail 1 n))))

(print "0! = " (fact 0) #\Newline)
(print "1! = " (fact 1) #\Newline)
(print "2! = " (fact 2) #\Newline)
(print "4! = " (fact 4) #\Newline)
(print "8! = " (fact 8) #\Newline)
(print "12! = " (fact 12) #\Newline)
