(define (loop i n)
  (if (>= i n)
      (print "A-done" #\Newline)
      (begin
        (print "A" i #\Newline)
        (thread-sleep! 5)
        (loop (+ i 1) n))))
(loop 0 5)
