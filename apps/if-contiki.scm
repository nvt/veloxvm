(define contiki?
  (lambda ()
    (equal? (substring (vector-ref (system-info) 1) 0 7) "Contiki")))

(if (contiki?)
  (print "Running Contiki" #\Newline)
  (print "Not running Contiki" #\Newline))
