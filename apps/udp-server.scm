; Create an empty list of client ports.
(define clients (list))

; Create a server port that listens on UDP port 8765.
(define server-sock (make-server 'UDP (vector 0) 5678))

; Add the next incoming client, if there is any.
(define handle-new-clients
  (lambda ()
    (if (incoming-client? server-sock)
	(let ((new-client (accept-client server-sock)))
	  (set! clients (cons new-client clients))
	  (print "Accepted a new client from "
		 (addr->string (peer-name new-client)) #\Newline)
	  (begin #t))
	(begin #f))))

(define process-command
  (lambda (client)
    (if (char-ready? client)
	(let ((msg (vector->string (read client))))
	  (print (addr->string (peer-name client))
		 ": \"" msg "\"" #\Newline)))))

(define process-clients
  (lambda ()
    (for-each process-command clients)))

; Main processing loop
(define processing-loop
  (lambda ()
    ; Sleep for 100 ms if there are no incoming clients.
    (if (not (handle-new-clients))
	(thread-sleep! 100))
    (process-clients)
    (processing-loop)))

(processing-loop)
