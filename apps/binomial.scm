(define fact-tail (lambda (result x)
  (if (<= x 1)
    result
    (fact-tail (* result x) (- x 1)))))

(define fact (lambda (n)
  (and (number? n) (fact-tail 1 n))))

(define binom (lambda (n k)
  (/ (fact n) (* (fact (- n k)) (fact k)))))

(print "fact(5) = " (fact 5) #\Newline)
(print "binom(5,2) = " (binom 5 2) #\Newline)
(print "binom(3,3) = " (binom 3 3) #\Newline)
(print "binom(15,11) = " (binom 15 11) #\Newline)
