(define sht11 (open-output-file "/dev/sht11"))

(define print-temp
  (lambda ()
    (print "Temperature: " (read sht11) #\Newline)
    (thread-sleep! 5000)
    (print-temp)))

(print-temp)
