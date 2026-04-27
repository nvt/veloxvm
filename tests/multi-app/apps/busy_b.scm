(define (loop i n)
  (if (>= i n)
      (print "B-done" #\Newline)
      (begin
        (print "B" i #\Newline)
        (loop (+ i 1) n))))
(loop 0 100)
