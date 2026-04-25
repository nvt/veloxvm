;; Enumerate every sensor registered with Contiki-NG's sensors library,
;; read a sample from each, and log the results.  Intended for the
;; Contiki-NG port -- the sensors library is a platform capability and
;; is imported rather than provided as a core primitive.

(import "sensors")

(define read-all
  (lambda (names)
    (if (null? names)
        #t
        (begin
          (print (car names) ": " (sensor-value (car names)) #\Newline)
          (read-all (cdr names))))))

(define scan
  (lambda ()
    (let ((names (get-sensors)))
      (print "Sensors: " (length names) #\Newline)
      (read-all names)
      (thread-sleep! 5000)
      (scan))))

(scan)
