;; Cond expression test
(define (sign x)
  (cond ((< x 0) -1)
        ((= x 0) 0)
        (else 1)))

(sign 5)
