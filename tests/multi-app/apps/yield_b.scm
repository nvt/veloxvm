(define (loop i n)
  (if (>= i n)
      (print "B-done" #\Newline)
      (begin
        (print "B" i #\Newline)
        (thread-sleep! 5)
        (loop (+ i 1) n))))
(loop 0 5)
