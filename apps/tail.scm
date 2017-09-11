(define counter (lambda (x)
  (if (< x 1000000)
    (counter (+ x 1))
    x)))

(counter 0)
