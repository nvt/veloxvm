(define (sum-up n acc)
  (if (<= n 0) acc (sum-up (- n 1) (+ acc n))))
(define (loop i n)
  (if (>= i n)
      (print "CB-done" #\Newline)
      (begin
        (print "CB" i "=" (sum-up 100 0) #\Newline)
        (loop (+ i 1) n))))
(loop 0 8)
