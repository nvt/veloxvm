(define dio-packet-fields (vector 8 8 16 1 1 3 3 8 8 8 128))

(define file (open-output-file "packet-log.txt"))
(define buffer
  (let ((rpl-instance-id 0) (version 0) (rank 256) (global 1)
	(mop 0) (prf 0) (dtsn 0) (flags 0)
	(dodag-id (vector #xaa #xaa 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
    (construct-packet
     dio-packet-fields
     (vector rpl-instance-id version rank global 0
	     mop prf dtsn flags 0 dodag-id))))
(deconstruct-packet dio-packet-fields buffer)
(buffer? buffer)
(write buffer file)
(close-output-port file)

