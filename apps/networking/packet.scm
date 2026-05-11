(define dio-packet-fields (vector 8 8 16 1 1 3 3 8 8 8 128))

(define (make-dodag-id)
  ;; Built via make-vector + vector-set! because the VM caps form arity
  ;; (VM_OBJECT_STACK_SIZE) below the 16 bytes a DODAG-ID needs.
  (let ((v (make-vector 16 0)))
    (vector-set! v 0 #xaa)
    (vector-set! v 1 #xaa)
    (vector-set! v 15 1)
    v))

(define file (open-output-file "packet-log.txt"))
(define buffer
  (let ((rpl-instance-id 0) (version 0) (rank 256) (global 1)
	(mop 0) (prf 0) (dtsn 0) (flags 0)
	(dodag-id (make-dodag-id)))
    (construct-packet
     dio-packet-fields
     (vector rpl-instance-id version rank global 0
	     mop prf dtsn flags 0 dodag-id))))
(define parsed (deconstruct-packet dio-packet-fields buffer))
(display "buffer: ") (display buffer) (newline)
(display "parsed: ") (display parsed) (newline)
(write buffer file)
(close-output-port file)

