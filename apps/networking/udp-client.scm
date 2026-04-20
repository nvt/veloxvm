;; Configuration
(define wait-time 10000)
(define send-interval 1000)
(define remote-ip-address (vector #xaaaa 0 0 0 #x200 0 0 1))
(define local-udp-port 8765)
(define remote-udp-port 5678)

;; Sleep for a certain amount of time specified in milliseconds,
;; and print the actual time that we sleep.
(define print-sleep
  (lambda (ms-interval)
;;    (print "Sleeping for " ms-interval " ms" #\Newline)
    (thread-sleep! ms-interval)))

;; Define the send loop. We sleep for a random time after sending
;; a message, with the average sleep time being approximately the
;; same as is specified in the configuration variable "send-interval".
(define send-loop
  (lambda (counter)
    (let ((msg (string-append "Hello " (number->string counter))))
      (print "DATA send to 1 '" msg "'" #\Newline)
      (write msg sock))
    (print-sleep (+ (/ send-interval 2) (modulo (random) send-interval)))
    (send-loop (+ counter 1))))

;; Sleep for an initial time to let the routing topology form.
(print-sleep wait-time)

;; Create a UDP socket.
(define sock
  (make-client 'UDP remote-ip-address remote-udp-port local-udp-port))

;; Initiate the send loop.
(send-loop 1)
