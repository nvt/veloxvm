(define (loop i n)
  (if (>= i n)
      (print "A-done" #\Newline)
      (begin
        (print "A" i #\Newline)
        (loop (+ i 1) n))))
(loop 0 100)
